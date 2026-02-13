#!/usr/bin/env bash
set -euo pipefail

# ========== 設定 ==========
MU_MANUAL="${MU_MANUAL:-}"   # 空なら自動。例: "0.005"

# 時間パラメータ
T0=100
T1=5000
SAMPLE_TS=100
TIME_STEP=100
HALF_STEP=$(( TIME_STEP / 2 ))

# Python
PYTHON_BIN="${PYTHON_BIN:-python3}"

# スクリプトパス
PREP_SCRIPT="/home/tomoya/Work/GitHub/SmokeRingVis/App/InSituVis.mpi_iso/for_4dgs/shift_and_prep_raw_iso.py"
CONV_SCRIPT="/home/tomoya/Work/GitHub/SmokeRingVis/App/InSituVis.mpi_iso/for_4dgs/convert_from_insitu_to_4dgs_iso.py"

# データルート
# MU_ROOT/mu0.005/params (json)
# MU_ROOT/mu0.005/1201/viewpoints (json)
# MU_ROOT/mu0.005/1201/Process0000 (bmp)
MU_ROOT="/data2/tomoya/SmokeRingIso"

# 出力先
RAW_BASE="/data2/tomoya/VisGaussian/SmokeRingIso/raw"
PROC_BASE="/data2/tomoya/VisGaussian/SmokeRingIso/processed"

# キャッシュ (muをまたいで共有するためルートに配置)
CACHE_DIR="/data2/tomoya/VisGaussian/SmokeRingIso/_shared_iso_cache"
# ==========================

# 事前チェック
[ -f "$PREP_SCRIPT" ] || { echo "[ERR] $PREP_SCRIPT not found" >&2; exit 1; }
[ -f "$CONV_SCRIPT" ] || { echo "[ERR] $CONV_SCRIPT not found" >&2; exit 1; }
[ -d "$MU_ROOT" ]     || { echo "[ERR] MU_ROOT not found: $MU_ROOT" >&2; exit 1; }

# ==========================
# mu ディレクトリ列挙
# ==========================
MU_DIRS=()
if [ -n "$MU_MANUAL" ]; then
  cand="${MU_ROOT}/mu${MU_MANUAL}"
  [ -d "$cand" ] || { echo "[ERR] manual mu dir not found: $cand" >&2; exit 1; }
  MU_DIRS+=( "$cand" )
else
  for d in "$MU_ROOT"/mu*; do
    [ -d "$d" ] || continue
    MU_DIRS+=( "$d" )
  done
fi

if [ ${#MU_DIRS[@]} -eq 0 ]; then
  echo "[ERR] No mu dirs found under $MU_ROOT" >&2; exit 1
fi

echo "[i] detected mu dirs:"
printf '  - %s\n' "${MU_DIRS[@]}"

# ==========================
# Main Loop
# ==========================
for mu_dir in "${MU_DIRS[@]}"; do
  mu_base="$(basename "$mu_dir")"  # mu0.005
  MU="${mu_base#mu}"              # 0.005

  # Isoデータは mu_dir 直下に params がある
  PARAMS_DIR="${mu_dir}/params"

  echo ""
  echo "======================================================="
  echo "[MU] $MU"
  echo "======================================================="

  if [ ! -d "$PARAMS_DIR" ]; then
    echo "[WARN] params dir not found: $PARAMS_DIR (skip MU)" >&2
    continue
  fi

  # view列挙 (mu_dir以下の数字ディレクトリなどを探索)
  VIEW_DIRS=()
  for d in "$mu_dir"/*; do
    [ -d "$d" ] || continue
    # viewpoints があるフォルダをViewとみなす
    if [ -d "$d/viewpoints" ]; then
      VIEW_DIRS+=( "$d" )
    fi
  done

  if [ ${#VIEW_DIRS[@]} -eq 0 ]; then
    echo "[WARN] No view dirs found in $mu_dir" >&2
    continue
  fi

  for view_dir in "${VIEW_DIRS[@]}"; do
    view_id="$(basename "$view_dir")"
    NAME="mu${MU}_${view_id}"
    OUT_RAW_DIR="${RAW_BASE}/${NAME}"

    echo ""
    echo "[VIEW] $view_id (MU=$MU)"
    echo "  - Raw Out: $OUT_RAW_DIR"

    # 1. Preprocess (Merge, Resize, Shift, Rename)
    #    Isoは params が mu直下、画像等は view直下 という構成
    "$PYTHON_BIN" "$PREP_SCRIPT" \
      --params-dir "$PARAMS_DIR" \
      --view-dir   "$view_dir" \
      --out-dir    "$OUT_RAW_DIR" \
      --cache-dir  "$CACHE_DIR"

    # 2. Conversion
    # (A) Normal
    OUT_PROC_NORMAL="${PROC_BASE}/${NAME}_${T0}to${T1}"
    echo "  [NORMAL] -> $OUT_PROC_NORMAL"
    "$PYTHON_BIN" "$CONV_SCRIPT" \
      --src-root "$OUT_RAW_DIR" \
      --out-dir  "$OUT_PROC_NORMAL" \
      --t0 "$T0" --t1 "$T1" \
      --sample-ts "$SAMPLE_TS" \
      --mu "$MU" \
      --time-step "$TIME_STEP"

    # (B) Eval
    OUT_PROC_EVAL="${PROC_BASE}/${NAME}_sp${HALF_STEP}_${T0}to${T1}"
    echo "  [EVAL]   -> $OUT_PROC_EVAL"
    "$PYTHON_BIN" "$CONV_SCRIPT" \
      --src-root "$OUT_RAW_DIR" \
      --out-dir  "$OUT_PROC_EVAL" \
      --t0 "$T0" --t1 "$T1" \
      --sample-ts "$SAMPLE_TS" \
      --mu "$MU" \
      --time-step "$TIME_STEP" \
      --eval_mode

  done

  # キャッシュ掃除（MUが変わればソース形状が変わるため、MU単位で消しても良いが
  # ID管理されているので消さなくても動く。ディスク節約のため消すならここで）
  # rm -rf "$CACHE_DIR"
done

echo ""
echo "[OK] All Done."