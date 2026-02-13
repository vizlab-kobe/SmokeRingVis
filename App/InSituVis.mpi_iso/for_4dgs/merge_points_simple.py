#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
For each mu directory under BASE_ROOT:
  - read isosurf_*_rank*.json from muX/params
  - merge per timestep -> isosurf_merged_{ts}.json
  - convert to PLY -> isosurf_merged_{ts}.ply
  - write all outputs to muX/params_merge

Usage:
  python merge_params_to_ply_all_mu.py \
      --base-root /home/tomoya/Work/Data/tomoya/CUBE
"""

import os, re, glob, json, argparse
from collections import defaultdict
from typing import List, Dict
import numpy as np
from plyfile import PlyData, PlyElement


# ----------------------
# Helper: 配列リサイズ用
# ----------------------
def resize_attribute(src_array: np.ndarray, target_len: int, default_val=0) -> np.ndarray:
    """
    src_array を target_len に合わせて引き延ばす（あるいは縮める）。
    - len(src) == target_len: そのまま返す
    - len(src) == 1: ブロードキャスト的に複製
    - len(src) > 1: インデックスを均等割りして引き延ばす (例: 2個 -> 前半/後半)
    - len(src) == 0: ゼロ埋め (default_val)
    """
    n_src = len(src_array)
    
    # 長さが一致していれば何もしない
    if n_src == target_len:
        return src_array

    # データの次元数確認 (N, 3) なのか (N,) なのか
    if src_array.ndim > 1:
        shape_rest = src_array.shape[1:]
    else:
        shape_rest = ()

    # ケースA: データが無い場合 -> ゼロ埋めなどで返す
    if n_src == 0:
        return np.full((target_len,) + shape_rest, default_val, dtype=src_array.dtype)

    # ケースB: データがある場合 -> インデックス計算で引き延ばし
    # np.linspace で 0 から n_src までのインデックスを target_len 個作る
    # astype(int) で切り捨てられるため、
    # 例: src=2, tgt=10 -> 0,0,0,0,0, 1,1,1,1,1 となる
    indices = np.linspace(0, n_src, target_len, endpoint=False).astype(int)
    
    return src_array[indices]


# ----------------------
# 既存ロジック
# ----------------------

def parse_time_step_from_filename(filename: str):
    """ファイル名から6桁のタイムステップを取り出す"""
    m = re.search(r'isosurf_(\d{6})_rank\d{3}\.json', filename)
    return m.group(1) if m else None

def load_json(filepath: str):
    with open(filepath, "r") as f:
        return json.load(f)

def merge_json_group(files: List[str]) -> Dict:
    """同じ time_step のファイル群を統合"""
    merged = {"time_step": None, "coords": [], "colors": [], "normals": []}
    for path in sorted(files):
        data = load_json(path)
        if merged["time_step"] is None:
            merged["time_step"] = data.get("time_step")
        elif merged["time_step"] != data.get("time_step"):
            print(f"Warning: mismatched time_step in {path}")
        
        # 単純結合（この時点では長さ不一致のまま結合される可能性があるが、PLY化時に直す）
        merged["coords"].extend(data.get("coords", []))
        merged["colors"].extend(data.get("colors", []))
        merged["normals"].extend(data.get("normals", []))
    return merged

def merge_all(input_dir: str, output_dir: str):
    """
    全 time_step を対象にマージして保存
    """
    os.makedirs(output_dir, exist_ok=True)
    files = glob.glob(os.path.join(input_dir, "isosurf_*_rank*.json"))
    timestep_map = defaultdict(list)

    for f in files:
        ts = parse_time_step_from_filename(os.path.basename(f))
        if not ts:
            continue
        out_path = os.path.join(output_dir, f"isosurf_merged_{ts}.json")
        if os.path.exists(out_path):
            continue
        timestep_map[ts].append(f)

    for ts, group in timestep_map.items():
        merged = merge_json_group(group)
        out_path = os.path.join(output_dir, f"isosurf_merged_{ts}.json")
        with open(out_path, "w") as out:
            json.dump(merged, out, indent=2)
        print(f"[✓] Wrote merged file timestep {ts}: {out_path}")

def store_ply_from_json(json_path: str, ply_path: str, skip_if_exists: bool = True):
    """isosurf_merged_{ts}.json から PLY を生成 (長さ不一致対応版)"""
    if skip_if_exists and os.path.exists(ply_path):
        print(f"[→] Skip (already exists): {ply_path}")
        return 0

    with open(json_path, "r") as f:
        data = json.load(f)

    # 1. まずデータをnumpy配列化
    coords  = np.array(data.get("coords", []),  dtype=np.float32)
    colors  = np.array(data.get("colors", []),  dtype=np.float32)
    normals = np.array(data.get("normals", []), dtype=np.float32)

    target_len = len(coords)

    # coordsが空なら空のPLYを作成して終了
    if target_len == 0:
        dtype = [
            ('x','f4'),('y','f4'),('z','f4'),
            ('nx','f4'),('ny','f4'),('nz','f4'),
            ('red','u1'),('green','u1'),('blue','u1')
        ]
        elements = np.empty(0, dtype=dtype)
        vertex_element = PlyElement.describe(elements, 'vertex')
        ply_data = PlyData([vertex_element], text=True)
        ply_data.write(ply_path)
        print(f"[✓] PLY written (empty): {ply_path}")
        return 0

    # 2. 長さ調整 (coordsを正として colors, normals をリサイズ)
    #    - Normals の調整
    if len(normals) != target_len:
        # print(f"  [Fix] Resizing normals: {len(normals)} -> {target_len}")
        normals = resize_attribute(normals, target_len, default_val=0.0)

    #    - Colors の調整
    if len(colors) != target_len:
        # print(f"  [Fix] Resizing colors: {len(colors)} -> {target_len}")
        colors = resize_attribute(colors, target_len, default_val=1.0) # 色がない場合は白(1.0)などで埋める想定

    # 3. 色の変換 (0-1 -> 0-255)
    if colors.size > 0 and colors.max() <= 1.0:
        colors = (colors * 255).clip(0, 255)
    colors = colors.astype(np.uint8)

    # 4. PLYデータ構築
    dtype = [
        ('x','f4'),('y','f4'),('z','f4'),
        ('nx','f4'),('ny','f4'),('nz','f4'),
        ('red','u1'),('green','u1'),('blue','u1')
    ]
    
    attributes = np.concatenate([coords, normals, colors], axis=1)
    elements = np.empty(target_len, dtype=dtype)
    elements[:] = list(map(tuple, attributes))

    vertex_element = PlyElement.describe(elements, 'vertex')
    ply_data = PlyData([vertex_element], text=True)
    ply_data.write(ply_path)
    print(f"[✓] PLY written: {ply_path} (N={target_len})")
    
    return target_len


# ----------------------
# muごとに回す部分
# ----------------------

def process_one_mu(mu_dir: str, params_name="params", out_name="params_merge"):
    input_dir = os.path.join(mu_dir, params_name)
    output_dir = os.path.join(mu_dir, out_name)

    if not os.path.isdir(input_dir):
        print(f"[WARN] params not found, skip: {input_dir}")
        return

    os.makedirs(output_dir, exist_ok=True)

    print("")
    print("====================================================")
    print(f"[MU] {os.path.basename(mu_dir)}")
    print(f"[IN ] {input_dir}")
    print(f"[OUT] {output_dir}")
    print("====================================================")

    # 1) merge json
    merge_all(input_dir, output_dir)

    # 2) merged json -> ply
    merged_jsons = sorted(glob.glob(os.path.join(output_dir, "isosurf_merged_*.json")))
    if not merged_jsons:
        print(f"[WARN] no merged json found in {output_dir}")
        return

    max_coord_num = 0
    max_ts_ply = None

    for jp in merged_jsons:
        ply_path = os.path.splitext(jp)[0] + ".ply"
        try:
            n = store_ply_from_json(jp, ply_path)
            if n > max_coord_num:
                max_coord_num = n
                max_ts_ply = ply_path
        except Exception as e:
            print(f"[×] Failed ply convert {jp}: {e}")
            import traceback
            traceback.print_exc()

    print(f"[i] max_coord_timestep ply: {max_ts_ply} (n={max_coord_num})")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--base-root", required=True,
                    help="mu* が並ぶルート。例: /home/tomoya/Work/Data/tomoya/CUBE")
    ap.add_argument("--mu-manual", default="",
                    help="空なら全mu。例: 0.0 を指定すると mu0.0 のみ。")
    args = ap.parse_args()

    base_root = args.base_root

    if not os.path.isdir(base_root):
        raise RuntimeError(f"base-root not found: {base_root}")

    # mu dirs listup
    mu_dirs = []
    if args.mu_manual:
        cand = os.path.join(base_root, f"mu{args.mu_manual}")
        if not os.path.isdir(cand):
            raise RuntimeError(f"manual mu dir not found: {cand}")
        mu_dirs = [cand]
    else:
        mu_dirs = [d for d in glob.glob(os.path.join(base_root, "mu*")) if os.path.isdir(d)]

    if not mu_dirs:
        raise RuntimeError(f"no mu dirs under {base_root}")

    print("[i] detected mu dirs:")
    for d in mu_dirs:
        print("  -", d)

    for mu_dir in mu_dirs:
        process_one_mu(mu_dir)

    print("")
    print("[OK] all done.")


if __name__ == "__main__":
    main()