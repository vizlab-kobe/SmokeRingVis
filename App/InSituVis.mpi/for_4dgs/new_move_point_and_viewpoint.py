# -*- coding: utf-8 -*-
import json, glob, os, re, argparse, csv, shutil
from typing import Sequence, Optional, List, Dict, Tuple
from collections import defaultdict
import numpy as np
from plyfile import PlyData, PlyElement
import matplotlib.pyplot as plt
from pathlib import Path
import hashlib

# ========== 基本ユーティリティ ==========
def _ensure_dir(p: str):
    os.makedirs(p, exist_ok=True)

def load_json(filepath: str):
    try:
        with open(filepath, "r") as f:
            return json.load(f)
    except Exception as e:
        print(f"[WARN] Failed to parse JSON: {filepath} ({e})")
        return None

def save_json(file_path: str, data: dict):
    _ensure_dir(os.path.dirname(file_path))
    with open(file_path, "w") as f:
        json.dump(data, f, ensure_ascii=False, indent=2, sort_keys=True)

def parse_time_step_from_points(filename: str) -> Optional[str]:
    m = re.search(r'points_(\d{6})_VolumeSamplingPoints\.json$', filename)
    return m.group(1) if m else None

# ========== 平行移動 ==========
def parallel_move_points(points_file: str, output_path: str, move: Sequence[float]) -> bool:
    data = load_json(points_file)
    if data is None:
        print(f"[SKIP] Invalid JSON, skip: {points_file}")
        return False
    mx, my, mz = move
    coords = data.get("coords", [])
    new_coords = []
    for p in coords:
        if isinstance(p, (list, tuple)) and len(p) == 3:
            x, y, z = p
            new_coords.append([x - mx, y - my, z - mz])
    data["coords"] = new_coords
    save_json(output_path, data)
    return True

def parallel_move_viewpoints(viewpoint_file: str, output_path: str) -> Sequence[float]:
    vp = load_json(viewpoint_file)
    if vp is None:
        raise RuntimeError(f"invalid viewpoint json: {viewpoint_file}")
    move = vp.get("look_at", [0, 0, 0])
    # 原点注視化
    vp["look_at"] = [0, 0, 0]
    x, y, z = vp.get("camera_position", [0, 0, 0])
    vp["camera_position"] = [x - move[0], y - move[1], z - move[2]]
    # rotation は使わない
    vp.pop("rotation", None)
    save_json(output_path, vp)
    return move

# ========== 画像 ==========
def _collect_bmps(view_dir: str) -> List[str]:
    # 直下 or Process0000 の両対応
    cands = []
    cands += glob.glob(os.path.join(view_dir, "output_*.bmp"))
    proc = os.path.join(view_dir, "Process0000")
    if os.path.isdir(proc):
        cands += glob.glob(os.path.join(proc, "*_color_*.bmp"))
        if not cands:
            cands += glob.glob(os.path.join(proc, "*.bmp"))
    return sorted(set(cands))

def copy_image(view_dir: str, out_dir: str):
    dst = os.path.join(out_dir, "img")
    _ensure_dir(dst)
    copied = 0
    for src in _collect_bmps(view_dir):
        base = os.path.basename(src)
        new_name = base.replace("_color_", "")
        dst_path = os.path.join(dst, new_name)
        if os.path.exists(dst_path):
            continue
        shutil.copy2(src, dst_path)
        copied += 1
    print(f"[✓] Copied {copied} images -> {dst}")

# ========== PLY 変換 ==========
def store_ply_from_json(json_path: str, ply_path: str, skip_if_exists: bool = True) -> int:
    if skip_if_exists and os.path.exists(ply_path):
        print(f"[→] Skip (exists): {ply_path}")
        return 0
    with open(json_path, "r") as f:
        data = json.load(f)

    coords  = np.array(data.get("coords", []),  dtype=np.float32)
    colors  = np.array(data.get("colors", []),  dtype=np.float32)
    normals = np.array(data.get("normals", []), dtype=np.float32)
    if not (len(coords) == len(colors) == len(normals)):
        raise ValueError("coords/colors/normals length mismatch")

    if colors.size > 0 and colors.max() <= 1.0:
        colors = (colors * 255).clip(0, 255).astype(np.uint8)
    else:
        colors = colors.astype(np.uint8)

    dtype = [('x','f4'),('y','f4'),('z','f4'),
             ('nx','f4'),('ny','f4'),('nz','f4'),
             ('red','u1'),('green','u1'),('blue','u1')]
    attributes = np.concatenate([coords, normals, colors], axis=1)
    elements = np.empty(coords.shape[0], dtype=dtype)
    elements[:] = list(map(tuple, attributes))
    PlyData([PlyElement.describe(elements, 'vertex')], text=True).write(ply_path)
    print(f"[✓] PLY wrote: {ply_path}")
    return len(coords)

def convert_all_points_jsons_to_ply(points_dir: str):
    jsons = sorted(glob.glob(os.path.join(points_dir, "volume_*.json")))
    if not jsons:
        print(f"[!] No volume_*.json in: {points_dir}")
        return
    for jp in jsons:
        ply = os.path.splitext(jp)[0] + ".ply"
        try:
            store_ply_from_json(jp, ply)
        except Exception as e:
            print(f"[×] fail {jp} -> .ply ({e})")

# ========== 集計 ==========
def load_points_counts(points_dir: str) -> Tuple[List[int], List[int]]:
    ts, cnt = [], []
    for jp in glob.glob(os.path.join(points_dir, "volume_*.json")):
        m = re.search(r"volume_(\d{6})\.json$", os.path.basename(jp))
        if not m: continue
        t = int(m.group(1))
        try:
            with open(jp, "r") as f: data = json.load(f)
            n = len(data.get("coords", []))
        except Exception as e:
            print(f"[!] read fail {jp}: {e}"); continue
        ts.append(t); cnt.append(n)
    order = sorted(range(len(ts)), key=lambda i: ts[i])
    return [ts[i] for i in order], [cnt[i] for i in order]

def save_points_counts_csv(timesteps, counts, out_csv):
    _ensure_dir(os.path.dirname(out_csv))
    with open(out_csv, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["timestep", "num_points"])
        for t, c in zip(timesteps, counts):
            w.writerow([t, c])
    print(f"[✓] CSV: {out_csv}")

def plot_points_counts(timesteps, counts, out_png):
    if not timesteps: print("[!] No data to plot."); return
    plt.figure(); plt.plot(timesteps, counts, marker="o")
    plt.xlabel("timestep"); plt.ylabel("#points"); plt.title("Number of points over timesteps")
    plt.grid(True); plt.tight_layout(); plt.savefig(out_png, dpi=200); plt.close()
    print(f"[✓] PNG: {out_png}")

# ========== 共有キャッシュ（moveごとに1回だけ平行移動 & PLY生成） ==========
def _move_id_from_vector(move: Sequence[float], digits: int = 6) -> str:
    """moveベクトルを丸めて安定IDにする（異なる表記の同値ベクトルを同一視）"""
    mx, my, mz = [round(float(v), digits) for v in move]
    s = f"{mx},{my},{mz}"
    # 衝突を避けるためにハッシュも付与
    return f"m_{mx}_{my}_{mz}__{hashlib.sha1(s.encode()).hexdigest()[:8]}"

def _copy_tree(src_dir: str, dst_dir: str):
    _ensure_dir(dst_dir)
    for p in sorted(Path(src_dir).glob("*")):
        sp = str(p)
        dp = os.path.join(dst_dir, p.name)
        if p.is_dir():
            _copy_tree(sp, dp)
        else:
            if not os.path.exists(dp):
                shutil.copy2(sp, dp)

def get_or_build_moved_points(points_root: str, move: Sequence[float],
                              cache_root: str, out_points_dir: str):
    """
    moveベクトル単位でキャッシュ。初回だけ points_root から -move して JSON/PLY を構築。
    2回目以降はキャッシュから out_points_dir にコピーするだけ。
    """
    move_id = _move_id_from_vector(move)
    cache_dir = os.path.join(cache_root, move_id)
    cache_points = os.path.join(cache_dir, "points")
    built = False

    if not os.path.isdir(cache_points) or not list(Path(cache_points).glob("volume_*.json")):
        # 初回構築
        print(f"[i] build cache for move={move} -> {cache_points}")
        _ensure_dir(cache_points)
        pt_files = sorted(glob.glob(os.path.join(points_root, "points_*_VolumeSamplingPoints.json")))
        for f in pt_files:
            ts = parse_time_step_from_points(os.path.basename(f))
            if not ts:
                print(f"[!] skip (cannot parse ts): {f}")
                continue
            dst = os.path.join(cache_points, f"volume_{ts}.json")
            parallel_move_points(f, dst, move)
        # PLY もキャッシュ側で生成しておく
        convert_all_points_jsons_to_ply(cache_points)
        built = True
    else:
        print(f"[→] reuse cache: {cache_points}")

    # 利用側へコピー
    _copy_tree(cache_points, out_points_dir)
    return built, move_id

# ========== メイン処理 ==========
def process_one_view(view_dir: str, points_root: str, out_dir: str, shared_cache_root: str):
    # viewpoints 原点化 & 保存
    in_vp = os.path.join(view_dir, "viewpoints")
    out_vp = os.path.join(out_dir, "viewpoints")
    _ensure_dir(out_vp)

    vp_files = sorted(glob.glob(os.path.join(in_vp, "output_*.json")))
    move = None
    for vp in vp_files:
        move = parallel_move_viewpoints(vp, os.path.join(out_vp, os.path.basename(vp)))
    if move is None:
        raise RuntimeError(f"No viewpoint JSON found in: {in_vp}")

    # 共通 points → 共有キャッシュ経由で out_dir/points へ展開
    out_points = os.path.join(out_dir, "points")
    _ensure_dir(out_points)
    built, move_id = get_or_build_moved_points(points_root, move, shared_cache_root, out_points)
    if built:
        print(f"[✓] points cache built ({move_id}) and expanded to {out_points}")
    else:
        print(f"[✓] points copied from cache ({move_id}) -> {out_points}")

    # 画像コピー（直下/Process0000両対応）
    copy_image(view_dir, out_dir)

    # PLY は cache 側で既に作成済みなので、こちらでは追加処理不要
    # （out_points へも .ply がコピー済み）

    # 時系列集計（CSV/PNG）
    csv_path = os.path.join(out_dir, "points_count.csv")
    png_path = os.path.join(out_dir, "points_count.png")
    if not os.path.exists(csv_path):
        t, c = load_points_counts(out_points)
        save_points_counts_csv(t, c, csv_path)
        plot_points_counts(t, c, png_path)
    else:
        print(f"[SKIP] exists: {csv_path}")

def main():
    ap = argparse.ArgumentParser(description="Preprocess all views under Output and emit raw per view")
    ap.add_argument("--mu", required=True, type=str, help="mu 値（出力名の接頭に使用）")
    ap.add_argument("--base-in", default="/data2/tomoya/SmokeRing/Output", help="入力 Output のルート")
    ap.add_argument("--out-base", default="/data2/tomoya/VisGaussian/SmokeRing/raw", help="raw 出力のベース")
    ap.add_argument("--cache-base", default="/data2/tomoya/VisGaussian/SmokeRing/_shared_points_cache",
                    help="move ベクトルごとの共有キャッシュ格納場所")
    args = ap.parse_args()

    base_in = Path(args.base_in)
    out_base = Path(args.out_base)
    cache_base = Path(args.cache_base)
    points_root = base_in / "points"

    if not base_in.is_dir():
        raise FileNotFoundError(f"base-in not found: {base_in}")
    if not points_root.is_dir():
        raise FileNotFoundError(f"points root not found: {points_root}")
    _ensure_dir(str(cache_base))

    # Output 直下の view 候補を列挙（"points" / "Process0000" を除く）
    candidates = [p for p in base_in.iterdir() if p.is_dir() and p.name not in ("points", "Process0000")]

    # view_dir が有効かチェック：viewpoints があること
    view_dirs = [p for p in candidates if (p / "viewpoints").is_dir()]
    if not view_dirs:
        raise RuntimeError(f"No valid view directories under {base_in} (need viewpoints/)")

    print(f"[i] detected {len(view_dirs)} views under {base_in}")

    for vd in sorted(view_dirs, key=lambda x: x.name):
        view_id = vd.name              # 例: "143"
        name = f"mu{args.mu}_{view_id}"
        out_dir = out_base / name
        _ensure_dir(str(out_dir))
        print(f"\n[VIEW] {view_id} -> {out_dir}")
        process_one_view(str(vd), str(points_root), str(out_dir), str(cache_base))

if __name__ == "__main__":
    main()
