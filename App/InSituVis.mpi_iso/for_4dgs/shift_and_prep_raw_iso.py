#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
File: shift_and_prep_raw_iso.py
Purpose:
  1. params/isosurf_*.json (rank分割) をMerge & Resize & Shift -> points/*.ply
  2. viewpoints/output_*.json をShift -> viewpoints/*.json
  3. Process0000/*_color_.bmp をRename -> img/*.bmp
  4. Cache機能による高速化
"""

import os, glob, json, argparse, shutil, re, hashlib
from pathlib import Path
from collections import defaultdict
import numpy as np
import matplotlib.pyplot as plt
from plyfile import PlyData, PlyElement
from typing import Sequence, List

def _ensure_dir(path):
    os.makedirs(path, exist_ok=True)

def load_json(path):
    with open(path, "r") as f:
        return json.load(f)

def save_json(path, data):
    _ensure_dir(os.path.dirname(path))
    with open(path, "w") as f:
        json.dump(data, f, indent=2, ensure_ascii=False)

# ========== 配列リサイズ (Fix Mismatch) ==========
def resize_attribute(src_array: np.ndarray, target_len: int, default_val=0) -> np.ndarray:
    n_src = len(src_array)
    if n_src == target_len: return src_array
    if n_src == 0:
        shape_rest = src_array.shape[1:] if src_array.ndim > 1 else ()
        return np.full((target_len,) + shape_rest, default_val, dtype=src_array.dtype)
    # 引き延ばし
    indices = np.linspace(0, n_src, target_len, endpoint=False).astype(int)
    return src_array[indices]

# ========== Points Processing (Merge & Shift) ==========
def parse_ts_from_iso(filename: str):
    # isosurf_000100_rank000.json
    m = re.search(r'isosurf_(\d{6})_rank\d+\.json', filename)
    return int(m.group(1)) if m else None

def merge_and_shift_points(src_files: List[Path], dst_ply: Path, shift_vec: Sequence[float]):
    """
    同じtimestepの複数ランクファイルを結合 -> リサイズ -> シフト -> PLY保存
    """
    mx, my, mz = shift_vec
    
    # 1. Merge
    merged_coords = []
    merged_colors = []
    merged_normals = []
    
    for p in src_files:
        data = load_json(p)
        merged_coords.extend(data.get("coords", []))
        merged_colors.extend(data.get("colors", []))
        merged_normals.extend(data.get("normals", []))
        
    coords = np.array(merged_coords, dtype=np.float32)
    colors = np.array(merged_colors, dtype=np.float32)
    normals = np.array(merged_normals, dtype=np.float32)
    
    target_len = len(coords)
    if target_len == 0:
        # 空PLY作成
        dtype = [('x','f4'),('y','f4'),('z','f4'),('nx','f4'),('ny','f4'),('nz','f4'),('red','u1'),('green','u1'),('blue','u1')]
        elements = np.empty(0, dtype=dtype)
        PlyData([PlyElement.describe(elements, 'vertex')], text=True).write(str(dst_ply))
        return 0

    # 2. Resize
    if len(normals) != target_len:
        normals = resize_attribute(normals, target_len, default_val=0.0)
    if len(colors) != target_len:
        colors = resize_attribute(colors, target_len, default_val=1.0) # 白

    # 3. Shift
    coords[:, 0] -= mx
    coords[:, 1] -= my
    coords[:, 2] -= mz

    # 4. Save PLY
    # Color 0-1 -> 0-255
    if colors.size > 0 and colors.max() <= 1.0:
        colors = (colors * 255).clip(0, 255)
    colors = colors.astype(np.uint8)

    dtype = [
        ('x','f4'),('y','f4'),('z','f4'),
        ('nx','f4'),('ny','f4'),('nz','f4'),
        ('red','u1'),('green','u1'),('blue','u1')
    ]
    attributes = np.concatenate([coords, normals, colors], axis=1)
    elements = np.empty(target_len, dtype=dtype)
    elements[:] = list(map(tuple, attributes))

    PlyData([PlyElement.describe(elements, 'vertex')], text=True).write(str(dst_ply))
    return target_len

# ========== Cache Logic ==========
def _move_id(move: Sequence[float]) -> str:
    mx, my, mz = [round(float(v), 6) for v in move]
    s = f"{mx},{my},{mz}"
    return f"m_{mx}_{my}_{mz}__{hashlib.sha1(s.encode()).hexdigest()[:6]}"

def get_or_build_points(params_dir: Path, out_points_dir: Path, shift_vec: Sequence[float], cache_dir: Path):
    """
    params_dir(mu単位) + shift_vec で一意なキャッシュを作成/利用
    """
    # ソースディレクトリのハッシュ（muが違えば違うIDになるように）
    src_sig = hashlib.sha1(str(params_dir).encode()).hexdigest()[:8]
    move_sig = _move_id(shift_vec)
    
    cache_root = cache_dir / f"src_{src_sig}_{move_sig}"
    cache_points = cache_root / "points"
    
    if cache_points.is_dir() and list(cache_points.glob("*.ply")):
        print(f"  [CACHE] Hit: {cache_root}")
        # Copy to output
        _ensure_dir(out_points_dir)
        for p in cache_points.glob("*.ply"):
            shutil.copy2(p, out_points_dir / p.name)
        return

    print(f"  [CACHE] Miss. Building... {cache_root}")
    _ensure_dir(cache_points)
    
    # params_dir 内の全jsonをtimestepごとにグルーピング
    files = sorted(params_dir.glob("isosurf_*.json"))
    ts_map = defaultdict(list)
    for f in files:
        ts = parse_ts_from_iso(f.name)
        if ts is not None:
            ts_map[ts].append(f)
            
    # マージ & Shift & Save
    for ts, f_list in ts_map.items():
        # 出力名は convertスクリプトに合わせて 'isosurf_merged_{ts}.ply' とする
        dst_ply = cache_points / f"isosurf_merged_{ts:06d}.ply"
        merge_and_shift_points(f_list, dst_ply, shift_vec)
        
    # Copy to output
    _ensure_dir(out_points_dir)
    for p in cache_points.glob("*.ply"):
        shutil.copy2(p, out_points_dir / p.name)

# ========== Viewpoints & Images ==========
def process_viewpoints(view_dir: Path, out_vp_dir: Path, shift_vec: Sequence[float]) -> Sequence[float]:
    """
    viewpoints/output_{ts}_{camID}.json を読み込み、
    LookAt分だけShiftして保存。
    """
    vp_src = view_dir / "viewpoints"
    if not vp_src.is_dir():
        raise RuntimeError("viewpoints dir not found")
    
    _ensure_dir(out_vp_dir)
    mx, my, mz = shift_vec
    
    files = sorted(vp_src.glob("*.json"))
    # 最初のファイルから shift_vec を決定するために使う(外部で決定済みならそれを使う)
    
    for f in files:
        data = load_json(f)
        cx, cy, cz = data.get("camera_position", [0,0,0])
        data["camera_position"] = [cx - mx, cy - my, cz - mz]
        data["look_at"] = [0.0, 0.0, 0.0]
        # output_{ts}_{camID}.json はそのままの名前でコピー
        save_json(out_vp_dir / f.name, data)

def process_images(view_dir: Path, out_img_dir: Path):
    """
    Process0000/*_color_.bmp -> img/*.bmp (rename)
    Process0000/*_depth_.bmp -> img/depth/*.bmp (optional)
    """
    proc0 = view_dir / "Process0000"
    if not proc0.is_dir():
        # Process0000がない場合、直下のbmpを探す
        proc0 = view_dir
    
    _ensure_dir(out_img_dir)
    
    cnt = 0
    # RGB: output_000100_000000_color_.bmp -> output_000100_000000.bmp
    for bmp in sorted(proc0.glob("*_color_.bmp")):
        new_name = bmp.name.replace("_color_", "")
        dst = out_img_dir / new_name
        if not dst.exists():
            shutil.copy2(bmp, dst)
        cnt += 1

    # Depth (あれば)
    # output_000100_000000_depth_.bmp -> そのまま or depth/へ
    depth_dir = out_img_dir.parent / "depth" # raw/depth
    for bmp in sorted(proc0.glob("*_depth_.bmp")):
        _ensure_dir(depth_dir)
        dst = depth_dir / bmp.name
        if not dst.exists():
            shutil.copy2(bmp, dst)

    if cnt == 0:
        # _color_ がついてないパターンのフォールバック
        for bmp in sorted(proc0.glob("output_*.bmp")):
            if "_depth_" in bmp.name: continue
            if "_color_" in bmp.name: continue # 上で処理済み
            dst = out_img_dir / bmp.name
            if not dst.exists():
                shutil.copy2(bmp, dst)
            cnt += 1
            
    print(f"  [IMG] Copied {cnt} images")

# ========== Stats ==========
def generate_stats(points_dir: Path, out_dir: Path):
    csv_path = out_dir / "points_count.csv"
    if csv_path.exists(): return
    
    ts_list, cnt_list = [], []
    for p in sorted(points_dir.glob("*.ply")):
        # isosurf_merged_000100.ply
        m = re.search(r'_(\d{6})\.ply$', p.name)
        if not m: continue
        t = int(m.group(1))
        
        # Read header
        c = 0
        with open(p, "rb") as f:
            for _ in range(50):
                line = f.readline()
                if b"element vertex" in line:
                    c = int(line.split()[2])
                    break
                if b"end_header" in line: break
        ts_list.append(t)
        cnt_list.append(c)
        
    if not ts_list: return
    
    # Sort
    idx = np.argsort(ts_list)
    ts_list = np.array(ts_list)[idx]
    cnt_list = np.array(cnt_list)[idx]
    
    import csv
    with open(csv_path, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["timestep", "num_points"])
        for t, c in zip(ts_list, cnt_list):
            w.writerow([t, c])
            
    plt.figure()
    plt.plot(ts_list, cnt_list, marker="o")
    plt.title("Points count")
    plt.savefig(out_dir / "points_count.png")
    plt.close()

# ========== Main ==========
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--params-dir", required=True, type=Path, help="mu/params")
    ap.add_argument("--view-dir", required=True, type=Path, help="mu/1201")
    ap.add_argument("--out-dir", required=True, type=Path, help="raw/mu_1201")
    ap.add_argument("--cache-dir", required=True, type=Path)
    args = ap.parse_args()

    # 1. Determine Shift Vector (from first viewpoint)
    vp_dir = args.view_dir / "viewpoints"
    # first_vp = next(sorted(vp_dir.glob("*.json")), None)
    vp_files = sorted(vp_dir.glob("*.json"))
    first_vp = vp_files[0] if vp_files else None
    if not first_vp:
        raise RuntimeError(f"No viewpoint json in {vp_dir}")
    
    vp_data = load_json(first_vp)
    shift_vec = vp_data.get("look_at", [0,0,0])
    
    # 2. Points (Cache & Merge & Shift)
    #    Output: raw/points/*.ply
    get_or_build_points(args.params_dir, args.out_dir / "points", shift_vec, args.cache_dir)
    
    # 3. Viewpoints (Shift)
    #    Output: raw/viewpoints/*.json
    process_viewpoints(args.view_dir, args.out_dir / "viewpoints", shift_vec)
    
    # 4. Images (Rename & Copy)
    #    Output: raw/img/*.bmp
    process_images(args.view_dir, args.out_dir / "img")
    
    # 5. Stats
    generate_stats(args.out_dir / "points", args.out_dir)

if __name__ == "__main__":
    main()