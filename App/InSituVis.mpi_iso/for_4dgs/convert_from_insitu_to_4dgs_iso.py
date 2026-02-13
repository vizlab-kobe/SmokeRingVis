#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os, re, math, json, argparse, shutil
from pathlib import Path
from typing import Optional, Dict, List, Tuple
import numpy as np
from collections import defaultdict
from PIL import Image

def _collect_available_ply(src_root: Path) -> Dict[int, Path]:
    """
    {src_root}/points/ 下の PLY を探す。
    volume_XXXXXX.ply または isosurf_merged_XXXXXX.ply に対応
    """
    points_dir = src_root / "points"
    mapping: Dict[int, Path] = {}
    if points_dir.is_dir():
        for p in points_dir.iterdir():
            # どちらのパターンも許容
            m = re.match(r"(volume|isosurf_merged)_(\d{6})\.ply$", p.name)
            if m:
                mapping[int(m.group(2))] = p
    return mapping

def _nearest_timestep(target: int, candidates: List[int]) -> Optional[int]:
    if not candidates: return None
    return min(candidates, key=lambda t: (abs(t - target), t))

def _count_ply_vertices(ply_path: Path) -> Optional[int]:
    try:
        with open(ply_path, "rb") as f:
            for _ in range(256):
                line = f.readline()
                if not line: break
                if b"element vertex" in line:
                    return int(line.decode("utf-8", "ignore").split()[2])
                if b"end_header" in line: break
    except: pass
    return None

def _make_export_timesteps(t0: int, t1: int, step: int, eval_mode: bool) -> set[int]:
    if step <= 0: raise ValueError("step must be > 0")
    if not eval_mode:
        return {ts for ts in range(t0, t1 + 1, step)}
    anchor = step // 2
    first = t0 + anchor
    return {ts for ts in range(first, t1 + 1, step)}

# --- Geometry ---
def fov_to_focal_length(fov_deg: float, image_size: float) -> float:
    return image_size / (2.0 * np.tan(np.radians(fov_deg) / 2.0))

def fov_to_focal(fov_deg: float, img_h: int) -> float:
    return img_h / (2 * math.tan(math.radians(fov_deg) / 2))

def build_R_c2w_from_lookat_for_RH(position, look_at, up_vector):
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
    if np.linalg.det(R) < 0: R[:, 0] *= -1
    return R

def parse_image_filename(name: str) -> Tuple[int, int]:
    # output_000100_000000.bmp -> (100, 0)
    s = Path(name).stem # output_000100_000000
    m = re.match(r"^output_(\d{6})_(\d{6})$", s)
    if not m:
        # Fallback for just numbers
        m = re.match(r"^(\d{6})_(\d{6})$", s)
    if not m:
        raise ValueError(f"Invalid image filename pattern: {name}")
    return int(m.group(1)), int(m.group(2))

def load_camera_param_jsons(params_dir: Path) -> Dict[int, dict]:
    # output_000100_000000.json -> needs to find camera_index inside
    params = {}
    for p in sorted(params_dir.glob('*.json')):
        with open(p, 'r') as f:
            data = json.load(f)
        cam_id = data.get('camera_index')
        if cam_id is None:
            # try parsing from filename if key missing: output_ts_camID
            m = re.search(r'_(\d{6})\.json$', p.name)
            if m: cam_id = int(m.group(1))
        
        if cam_id is not None:
            params[int(cam_id)] = data
    return params

def build_pose_vec(R_c2w: np.ndarray, C_w: np.ndarray, H: int, W: int, focal: float) -> np.ndarray:
    right, up, forward = R_c2w[:, 0], R_c2w[:, 1], -R_c2w[:, 2]
    c2w = np.column_stack([right, up, forward])
    pose = np.zeros((3, 5), dtype=np.float32)
    pose[:, 1:2] = c2w[:, 0:1]
    pose[:, 0:1] = -c2w[:, 1:2]
    pose[:, 2:4] = np.column_stack([c2w[:, 2], C_w])
    pose[0, 4] = H; pose[1, 4] = W; pose[2, 4] = focal
    return pose.flatten()

def convert_from_insitu_to_4dgs(src_root: Path, out_dir: Path,
                                t0: int, t1: int, sample_ts: Optional[int],
                                jpg_quality: int, near: float, far: float,
                                max_cams: int, eval_mode: bool,
                                time_step: int, mu:float) -> dict:
    out_dir.mkdir(parents=True, exist_ok=True)

    img_dir = src_root / "img"
    bmp_files = sorted(img_dir.glob("output_*.bmp"))
    if not bmp_files: raise RuntimeError(f"No BMP found in {img_dir}")

    params_dir = src_root / "viewpoints"
    camera_params = load_camera_param_jsons(params_dir)

    export_ts_set = _make_export_timesteps(t0, t1, time_step, eval_mode)
    images_by_camera = defaultdict(list)
    for img_file in bmp_files:
        try:
            frame_ts, cam_id = parse_image_filename(img_file.name)
            if frame_ts in export_ts_set:
                images_by_camera[cam_id].append((frame_ts, img_file))
        except ValueError: pass

    if not images_by_camera: raise RuntimeError("No valid images found.")

    with Image.open(bmp_files[0]) as im0: width, height = im0.size
    
    # Intrinsics (Use first available camera)
    first_cam_id = sorted(images_by_camera.keys())[0]
    sample_param = camera_params[first_cam_id]
    fovX = float(sample_param.get('fovX'))
    fovY = float(sample_param.get('fovY', fovX))
    fx, fy = fov_to_focal_length(fovX, width), fov_to_focal_length(fovY, height)
    intrinsics_json = {
        "model": "PINHOLE", "width": width, "height": height,
        "fx": fx, "fy": fy, "cx": width/2.0, "cy": height/2.0,
        "fovX_deg": fovX, "fovY_deg": fovY
    }

    extrinsics_entries = []
    for cam_id in sorted(images_by_camera.keys()):
        if cam_id not in camera_params: continue
        param = camera_params[cam_id]
        
        pos = np.array(param['camera_position'], dtype=float)
        look = np.array(param['look_at'], dtype=float)
        up = np.array(param['up_vector'], dtype=float)
        
        R_c2w = build_R_c2w_from_lookat_for_RH(pos, look, up)
        t_wc = (-R_c2w.T @ pos).tolist()

        cam_out = out_dir / f"cam{cam_id:03d}"
        cam_out.mkdir(exist_ok=True)
        
        for idx, (_, bmp_path) in enumerate(sorted(images_by_camera[cam_id])):
            with Image.open(bmp_path) as img:
                if img.mode == 'RGBA': img = img.convert('RGB')
                img.save(cam_out / f"frame_{idx+1:05d}.jpg", 'JPEG', quality=jpg_quality)
        
        extrinsics_entries.append({
            "id": cam_id, "camera_id": cam_id, "name": f"cam{cam_id:03d}",
            "R": R_c2w.tolist(), "T": t_wc,
            "camera_position": param['camera_position'],
            "look_at": param['look_at'], "up_vector": param['up_vector']
        })

    cam_info = {
        "version": 1, "coordinate_system": "right-handed",
        "R_convention": "world_to_camera rows=[right, up, forward(+Z)]",
        "intrinsics": intrinsics_json, "entries": extrinsics_entries
    }
    with open(out_dir / 'cam_info.json', 'w') as f: json.dump(cam_info, f, indent=2)

    # PLY Copy
    if not eval_mode:
        ply_map = _collect_available_ply(src_root)
        in_range = {ts: p for ts, p in ply_map.items() if t0 <= ts <= t1}
        if in_range:
            if sample_ts and sample_ts in in_range:
                shutil.copy2(in_range[sample_ts], out_dir / f"{sample_ts:06d}.ply")
            
            mid_ts = _nearest_timestep((t0+t1)//2, sorted(in_range.keys()))
            if mid_ts: shutil.copy2(in_range[mid_ts], out_dir / f"middle_ts{mid_ts:06d}.ply")

            best_ts = max(in_range, key=lambda t: _count_ply_vertices(in_range[t]) or 0)
            shutil.copy2(in_range[best_ts], out_dir / f"largest_ts{best_ts:06d}.ply")

    timesteps = sorted(export_ts_set)
    sim_info = {
        "mu": mu, "time": {"min": t0, "max": t1, "split": time_step},
        "timestep_mapping": {str(i+1): ts for i, ts in enumerate(timesteps)}
    }
    with open(out_dir / "sim_param_info.json", "w") as f: json.dump(sim_info, f, indent=2)

    _make_poses_bounds(cam_info, out_dir, near, far, max_cams)
    return cam_info

def _make_poses_bounds(cam_info, out_dir, near, far, max_cams):
    H, W = int(cam_info["intrinsics"]["height"]), int(cam_info["intrinsics"]["width"])
    focal = fov_to_focal(cam_info["intrinsics"]["fovY_deg"], H)
    entries = cam_info["entries"]
    step = max(1, len(entries)//max_cams) if max_cams > 0 else 1
    
    poses = []
    for e in entries[::step]:
        R_wc = np.array(e["R"])
        t_wc = np.array(e["T"])
        R_c2w = R_wc.T
        C_w = -R_wc.T @ t_wc
        p = build_pose_vec(R_c2w, C_w, H, W, focal)
        poses.append(np.concatenate([p, [near, far]]))
    np.save(out_dir / "poses_bounds_multipleview.npy", np.stack(poses, 0))

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--src-root", required=True, type=Path)
    ap.add_argument("--out-dir", required=True, type=Path)
    ap.add_argument("--t0", type=int, required=True)
    ap.add_argument("--t1", type=int, required=True)
    ap.add_argument("--sample-ts", type=int, default=None)
    ap.add_argument("--mu", type=float, required=True)
    ap.add_argument("--time-step", type=int, default=100)
    ap.add_argument("--eval_mode", action="store_true")
    
    # defaults
    ap.add_argument("--jpg-quality", type=int, default=95)
    ap.add_argument("--near", type=float, default=0.1)
    ap.add_argument("--far", type=float, default=100.0)
    ap.add_argument("--max-cams", type=int, default=20)
    
    args = ap.parse_args()
    convert_from_insitu_to_4dgs(
        args.src_root, args.out_dir, args.t0, args.t1, args.sample_ts,
        args.jpg_quality, args.near, args.far, args.max_cams,
        args.eval_mode, args.time_step, args.mu
    )
    print("\n[✓] Conversion finished.")

if __name__ == "__main__":
    main()