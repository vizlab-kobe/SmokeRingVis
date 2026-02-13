#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
File: new_convert_to_visgaussian.py (single-file combined version)

Purpose:
  - (no-stage版) InSituの前処理済み raw dir から MultipleView 形式を生成
  - time_step / eval_mode による分割仕様を統一
  - PLY は points/ 配下から sample/middle/largest を選抜（eval_mode時はスキップ）

Input (src_root):
  RGB:         {src_root}/output_XXXXXX_YYYYYY.bmp
  depth(opt):  {src_root}/depth/output_XXXXXX_YYYYYY_depth_.bmp
  params:      {src_root}/params/*.json   (camera_index または stem)
  points:      {src_root}/points/isosurf_merged_XXXXXX.ply  or volume_XXXXXX.ply

Output (out_dir):
  cam###/frame_*.jpg
  cam###/depth/frame_*.png (depthがある場合のみ)
  cam_info.json
  sim_param_info.json
  poses_bounds_multipleview.npy
"""

import os, re, math, json, argparse, shutil
from pathlib import Path
from typing import Optional, Dict, List, Tuple, Set
from collections import defaultdict

import numpy as np
from PIL import Image


# =============================================================
# Helpers: points (PLY)
# =============================================================

def _collect_available_ply(src_root: Path) -> Dict[int, Path]:
    """
    {src_root}/points/*.ply を走査して {ts: path} を返す。
    volume_XXXXXX.ply / isosurf_merged_XXXXXX.ply の両対応。
    """
    points_dir = src_root / "points"
    mapping: Dict[int, Path] = {}
    if not points_dir.is_dir():
        return mapping

    for p in points_dir.iterdir():
        if not p.is_file():
            continue
        m1 = re.match(r"volume_(\d{6})\.ply$", p.name)
        m2 = re.match(r"isosurf_merged_(\d{6})\.ply$", p.name)
        m = m1 or m2
        if m:
            mapping[int(m.group(1))] = p
    return mapping


def _nearest_timestep(target: int, candidates: List[int]) -> Optional[int]:
    if not candidates:
        return None
    return min(candidates, key=lambda t: (abs(t - target), t))


def _count_ply_vertices(ply_path: Path) -> Optional[int]:
    """PLYヘッダの 'element vertex N' を読む"""
    try:
        with open(ply_path, "rb") as f:
            for _ in range(256):
                line = f.readline()
                if not line:
                    break
                if b"element vertex" in line:
                    parts = line.decode("utf-8", "ignore").split()
                    return int(parts[2]) if len(parts) >= 3 else None
                if b"end_header" in line:
                    break
    except Exception:
        pass
    return None


# =============================================================
# Helpers: timestep split
# =============================================================

def _make_export_timesteps(t0: int, t1: int, step: int, eval_mode: bool) -> Set[int]:
    """
    eval_mode=False:
        t0, t0+step, t0+2*step, ... <= t1

    eval_mode=True:
        t0 + step//2, t0 + step + step//2, ... <= t1
    """
    if step <= 0:
        raise ValueError("step must be > 0")

    if not eval_mode:
        return {ts for ts in range(t0, t1 + 1, step)}

    anchor = step // 2
    first = t0 + anchor
    return {ts for ts in range(first, t1 + 1, step)}


# =============================================================
# Helpers: camera / geometry
# =============================================================

def fov_to_focal_length(fov_deg: float, image_size: float) -> float:
    return image_size / (2.0 * np.tan(np.radians(fov_deg) / 2.0))


def fov_to_focal(fov_deg: float, img_h: int) -> float:
    return img_h / (2 * math.tan(math.radians(fov_deg) / 2))


def build_R_c2w_from_lookat_for_RH(position, look_at, up_vector):
    """右手系の R（camera->world, +Z forward）"""
    pos = np.asarray(position, dtype=float)
    look = np.asarray(look_at, dtype=float)
    up  = np.asarray(up_vector, dtype=float)

    forward = look - pos
    forward /= np.linalg.norm(forward)

    right = np.cross(forward, up)
    if np.linalg.norm(right) < 1e-12:
        tmp = np.array([1,0,0]) if abs(forward[0]) < 0.9 else np.array([0,1,0])
        right = np.cross(forward, tmp)
    right /= np.linalg.norm(right)

    new_up = np.cross(right, forward)
    new_up /= np.linalg.norm(new_up)

    R = np.column_stack([right, new_up, forward])
    if np.linalg.det(R) < 0:
        R[:, 0] *= -1
    return R


def parse_image_filename(name: str) -> Tuple[int, int]:
    """
    'output_XXXXXX_YYYYYY.bmp' / 'output_XXXXXX_YYYYYY' を想定して
    (frame_ts, cam_id) を返す。
    """
    s = Path(name).name
    m = re.match(r"^output_(\d{6})_(\d{6})(?:\.[A-Za-z0-9]+)?$", s)
    if not m:
        s2 = Path(name).stem
        m = re.match(r"^(\d{6})_(\d{6})$", s2)
    if not m:
        raise ValueError(f"Invalid image filename pattern: {name}")
    return int(m.group(1)), int(m.group(2))


def load_camera_param_jsons(params_dir: Path) -> Dict[int, dict]:
    """
    {src_root}/params/*.json を読み、camera_index があれば cam_id に。
    無ければ stem を cam_id として扱う（例: 000001.json -> 1）。
    """
    params: Dict[int, dict] = {}
    for p in sorted(params_dir.glob("*.json")):
        with open(p, "r") as f:
            data = json.load(f)

        cam_id_raw = data.get("camera_index")
        if cam_id_raw is None:
            try:
                cam_id = int(p.stem)
            except ValueError as e:
                raise ValueError(f"camera_index missing and filename not int: {p}") from e
        else:
            cam_id = int(cam_id_raw)

        params[cam_id] = data

    if not params:
        raise RuntimeError(f"No camera jsons found under: {params_dir}")
    return params


def build_pose_vec(R_c2w: np.ndarray, C_w: np.ndarray,
                   H: int, W: int, focal: float) -> np.ndarray:
    # LLFF準拠の軸入れ替え
    right, up, forward = R_c2w[:, 0], R_c2w[:, 1], -R_c2w[:, 2]
    c2w = np.column_stack([right, up, forward])

    pose = np.zeros((3, 5), dtype=np.float32)
    pose[:, 1:2] = c2w[:, 0:1]
    pose[:, 0:1] = -c2w[:, 1:2]
    pose[:, 2:4] = np.column_stack([c2w[:, 2], C_w])

    pose[0, 4] = H; pose[1, 4] = W; pose[2, 4] = focal
    return pose.flatten()


def _make_poses_bounds(cam_info: dict, out_dir: Path,
                       near: float, far: float, max_cams: int) -> int:
    H = int(cam_info["intrinsics"]["height"])
    W = int(cam_info["intrinsics"]["width"])
    fovY = float(cam_info["intrinsics"]["fovY_deg"])
    focal = fov_to_focal(fovY, H)

    entries = cam_info["entries"]
    step = max(1, len(entries) // max_cams) if max_cams > 0 else 1
    selected = entries[::step]

    poses_bounds = []
    for e in selected:
        R_wc = np.array(e["R"], dtype=np.float32)
        t_wc = np.array(e["T"], dtype=np.float32)

        C_w   = -R_wc.T @ t_wc
        R_c2w = R_wc.T

        pose_vec = build_pose_vec(R_c2w, C_w, H, W, focal)
        poses_bounds.append(np.concatenate([pose_vec, [near, far]], dtype=np.float32))

    poses_bounds = np.stack(poses_bounds, 0)
    np.save(out_dir / "poses_bounds_multipleview.npy", poses_bounds)
    return poses_bounds.shape[0]


# =============================================================
# Main conversion
# =============================================================

def convert_from_insitu_to_4dgs(
    src_root: Path,
    out_dir: Path,
    t0: int,
    t1: int,
    sample_ts: Optional[int],
    jpg_quality: int,
    near: float,
    far: float,
    max_cams: int,
    eval_mode: bool,
    time_step: int,
    mu: float,
) -> dict:
    """
    入力:
      RGB:        {src_root}/output_XXXXXX_YYYYYY.bmp  (img/ 無し)
      depth(任意):{src_root}/depth/output_XXXXXX_YYYYYY_depth_.bmp
      params:     {src_root}/params/*.json
      ply:        {src_root}/points/isosurf_merged_XXXXXX.ply or volume_XXXXXX.ply

    出力:
      cam###/frame_*.jpg
      cam###/depth/frame_*.png (depthがある場合のみ)
      cam_info.json, sim_param_info.json, poses_bounds_multipleview.npy
    """
    out_dir.mkdir(parents=True, exist_ok=True)

    # --- RGB画像は src_root 直下の bmp
    bmp_files = sorted(src_root.glob("output_*.bmp"))
    if not bmp_files:
        raise RuntimeError(f"No BMP found under src_root: {src_root}")

    # --- params は src_root/params
    params_dir = src_root / "params"
    if not params_dir.is_dir():
        raise FileNotFoundError(f"'params' not found under: {params_dir}")
    camera_params = load_camera_param_jsons(params_dir)

    # --- depth（任意）
    depth_dir = src_root / "depth"
    has_depth = depth_dir.is_dir()
    depth_files = set()
    if has_depth:
        depth_files = set(p.name for p in depth_dir.glob("*_depth_*.bmp"))

    # --- 画像を (frame_ts, cam_id) ごとに集約
    start_ts, end_ts = t0, t1
    images_by_camera = defaultdict(list)

    export_ts_set = _make_export_timesteps(start_ts, end_ts, time_step, eval_mode)
    print("[i] eval_mode =", eval_mode)
    print("[i] export timesteps =", sorted(export_ts_set))

    for img_file in bmp_files:
        frame_ts, cam_id = parse_image_filename(img_file.name)
        if frame_ts not in export_ts_set:
            continue
        images_by_camera[cam_id].append((frame_ts, img_file))

    if not images_by_camera:
        raise RuntimeError("No images fall into the specified timestep range.")

    # --- 画像サイズ
    with Image.open(bmp_files[0]) as im0:
        width, height = im0.size

    # --- 内部パラメータ（最初のカメラから）
    sample_param = next(iter(camera_params.values()))
    fovX = float(sample_param.get("fovX"))
    fovY = float(sample_param.get("fovY", fovX))
    fx = fov_to_focal_length(fovX, width)
    fy = fov_to_focal_length(fovY, height)
    cx, cy = width / 2.0, height / 2.0
    intrinsics_json = {
        "model": "PINHOLE",
        "width": width, "height": height,
        "fx": fx, "fy": fy, "cx": cx, "cy": cy,
        "fovX_deg": fovX, "fovY_deg": fovY
    }

    # --- BMP -> JPEG 変換 & 外部作成
    extrinsics_entries = []
    for cam_id in sorted(images_by_camera.keys()):
        if cam_id not in camera_params:
            raise KeyError(f"Camera param JSON missing for cam_id={cam_id} in {params_dir}")

        param = camera_params[cam_id]
        
        # ----------------------------------------------------
        # データ読み出し (リストとして保持)
        # ----------------------------------------------------
        raw_pos  = param["camera_position"]
        raw_look = param["look_at"]
        raw_up   = param["up_vector"]
        raw_rot  = param.get("rotation")  # rotation も取得

        # 計算用には numpy 配列化
        pos  = np.asarray(raw_pos, dtype=float)
        look = np.asarray(raw_look, dtype=float)
        up   = np.asarray(raw_up, dtype=float)

        R_c2w = build_R_c2w_from_lookat_for_RH(pos, look, up)
        C_w   = pos
        R_wc  = R_c2w.T
        t_wc  = (-R_wc @ C_w).tolist()

        cam_dir = out_dir / f"cam{cam_id:03d}"
        cam_dir.mkdir(exist_ok=True)

        cam_depth_dir = cam_dir / "depth"
        if has_depth:
            cam_depth_dir.mkdir(exist_ok=True)

        for idx, (_, bmp_path) in enumerate(sorted(images_by_camera[cam_id])):
            out_name = f"frame_{idx+1:05d}.jpg"
            with Image.open(bmp_path) as img:
                if img.mode == "RGBA":
                    img = img.convert("RGB")
                img.save(cam_dir / out_name, "JPEG", quality=jpg_quality)

            # depthは同じstemに _depth_.bmp が付く前提（任意）
            if has_depth:
                depth_name = f"{bmp_path.stem}_depth_.bmp"
                if depth_name in depth_files:
                    dpath = depth_dir / depth_name
                    try:
                        with Image.open(dpath) as dimg:
                            dimg.save(cam_depth_dir / f"frame_{idx+1:05d}.png")
                    except Exception as e:
                        print(f"[WARN] failed to save depth {dpath}: {e}")

        # ----------------------------------------------------
        # エントリ生成: ここに元の生値を追加
        # ----------------------------------------------------
        entry = {
            "id": cam_id,
            "camera_id": cam_id,
            "name": f"cam{cam_id:03d}",
            "R": R_c2w.tolist(),  # camera->world
            "T": t_wc,            # world->camera

            "camera_position": raw_pos,
            "look_at": raw_look,
            "up_vector": raw_up,
        }
        
        # rotation が存在する場合のみ追加（もしくは常にNoneでも入れるなら調整可能）
        if raw_rot is not None:
            entry["rotation"] = raw_rot

        extrinsics_entries.append(entry)

    cam_info = {
        "version": 1,
        "coordinate_system": "right-handed",
        "R_convention": "world_to_camera rows=[right, up, forward(+Z)]",
        "intrinsics": intrinsics_json,
        "entries": extrinsics_entries
    }
    with open(out_dir / "cam_info.json", "w") as f:
        json.dump(cam_info, f, indent=2)

    # --- PLY選抜（eval_mode時はスキップ）
    if not eval_mode:
        ply_map = _collect_available_ply(src_root)
        in_range = {ts: p for ts, p in ply_map.items() if start_ts <= ts <= end_ts}
        if in_range:
            if sample_ts is not None:
                sample_src = in_range.get(sample_ts)
                if sample_src and sample_src.exists():
                    shutil.copy2(sample_src, out_dir / f"{sample_ts:06d}.ply")
                    print(f"[✓] Copied sample: {sample_ts:06d}.ply")
                else:
                    print(f"[×] sample_ts {sample_ts:06d} not found in points/*.ply")

            center_ts = (start_ts + end_ts) // 2
            mid_ts = _nearest_timestep(center_ts, sorted(in_range.keys()))
            if mid_ts is not None:
                shutil.copy2(in_range[mid_ts], out_dir / f"middle_ts{mid_ts:06d}.ply")
                print(f"[✓] Saved middle: ts {mid_ts:06d}")

            best_ts, best_cnt = None, -1
            for ts, p in sorted(in_range.items()):
                cnt = _count_ply_vertices(p)
                if cnt is None:
                    cnt = p.stat().st_size
                if cnt > best_cnt:
                    best_cnt, best_ts = cnt, ts
            if best_ts is not None:
                shutil.copy2(in_range[best_ts], out_dir / f"largest_ts{best_ts:06d}.ply")
                print(f"[✓] Saved largest: ts {best_ts:06d} (score={best_cnt})")
        else:
            print("[i] No PLY found under points/ for given range; skip copying PLYs.")

    # --- timestep mapping
    timesteps = sorted(_make_export_timesteps(t0, t1, time_step, eval_mode))
    timestep_mapping = {str(i+1): ts for i, ts in enumerate(timesteps)}
    sim_param_info = {
        "mu": mu,
        "time": {"min": t0, "max": t1, "split": time_step},
        "timestep_mapping": timestep_mapping
    }
    with open(out_dir / "sim_param_info.json", "w") as f:
        json.dump(sim_param_info, f, indent=2)

    # --- poses_bounds
    nposes = _make_poses_bounds(cam_info, out_dir, near, far, max_cams)
    print(f"[i] poses_bounds_multipleview.npy -> {nposes} poses")

    return cam_info


# =============================================================
# CLI
# =============================================================

def main():
    ap = argparse.ArgumentParser(
        description="InSitu raw -> MultipleView (single-file, no stage, split-aware)"
    )
    ap.add_argument("--src-root", required=True, type=Path,
                    help="前処理済みルート（例: /data2/.../raw/NAME）")
    ap.add_argument("--out-dir", required=True, type=Path,
                    help="最終出力（MultipleView形式）")
    ap.add_argument("--t0", type=int, required=True)
    ap.add_argument("--t1", type=int, required=True)
    ap.add_argument("--sample-ts", type=int, default=None)
    ap.add_argument("--jpg-quality", type=int, default=95)
    ap.add_argument("--near", type=float, default=0.1)
    ap.add_argument("--far", type=float, default=100.0)
    ap.add_argument("--max-cams", type=int, default=20)
    ap.add_argument("--eval_mode", action="store_true")
    ap.add_argument("--time-step", type=int, default=10,
                    help="通常時の刻み幅。eval時は中点(step//2)のみ。")
    ap.add_argument("--mu", type=float, required=True)

    args = ap.parse_args()
    if args.t0 > args.t1:
        raise ValueError("--t0 must be <= --t1")
    if args.time_step <= 0:
        raise ValueError("--time-step must be > 0")

    convert_from_insitu_to_4dgs(
        src_root=args.src_root,
        out_dir=args.out_dir,
        t0=args.t0, t1=args.t1,
        sample_ts=args.sample_ts,
        jpg_quality=args.jpg_quality,
        near=args.near, far=args.far,
        max_cams=args.max_cams,
        eval_mode=args.eval_mode,
        time_step=args.time_step,
        mu=args.mu,
    )

    # 統計
    cam_info = json.load(open(args.out_dir / "cam_info.json"))
    num_cam = len(cam_info["entries"])
    num_img = sum(len(list((args.out_dir / e["name"]).glob("*.jpg")))
                  for e in cam_info["entries"])

    print("\n[✓] Conversion finished.")
    print(f"  Cameras : {num_cam}")
    print(f"  Images  : {num_img}")


if __name__ == "__main__":
    main()