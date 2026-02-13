#!/usr/bin/env bash
set -euo pipefail

# ========== 設定 ==========
MU_MANUAL="${MU_MANUAL:-}"   # 空なら自動で mu* 全部。指定すればそのmuだけ。

# 時間パラメータ
T0=0
T1=3000
SAMPLE_TS=100
TIME_STEP=100
HALF_STEP=$(( TIME_STEP / 2 ))

# Python
PYTHON_BIN="${PYTHON_BIN:-python3}"

# スクリプト
PREP_SCRIPT="/home/tomoya/Work/GitHub/SmokeRingVis/App/InSituVis.mpi/for_4dgs/new_move_point_and_viewpoint.py"
CONV_SCRIPT="/home/tomoya/Work/GitHub/SmokeRingVis/App/InSituVis.mpi/for_4dgs/convert_from_insitu_to_4dgs.py"

# mu ルート（ここ以下に mu0.03 / mu0.125 ... が並ぶ想定）
MU_ROOT="/data2/tomoya/SmokeRingPre"

# 中間/最終
RAW_BASE="/data2/tomoya/VisGaussian/SmokeRingPre/raw"
PROC_BASE="/data2/tomoya/VisGaussian/SmokeRingPre/processed"

# 転送先
REMOTE_USER="tomoyam"
REMOTE_HOST="10.34.33.218"
REMOTE_PORT=22
# ==========================

# 事前チェック
[ -f "$PREP_SCRIPT" ] || { echo "[ERR] $PREP_SCRIPT not found" >&2; exit 1; }
[ -f "$CONV_SCRIPT" ] || { echo "[ERR] $CONV_SCRIPT not found" >&2; exit 1; }
command -v "$PYTHON_BIN" >/dev/null 2>&1 || { echo "[ERR] python not found" >&2; exit 1; }
[ -d "$MU_ROOT" ] || { echo "[ERR] MU_ROOT not found: $MU_ROOT" >&2; exit 1; }

# ==========================
# mu ディレクトリ列挙
# ==========================
MU_DIRS=()

if [ -n "$MU_MANUAL" ]; then
  # 手動指定があればそれだけ回す
  cand="${MU_ROOT}/mu${MU_MANUAL}"
  if [ ! -d "$cand" ]; then
    echo "[ERR] manual mu dir not found: $cand" >&2
    exit 1
  fi
  MU_DIRS+=( "$cand" )
else
  # 自動で mu* を全部拾う
  for d in "$MU_ROOT"/mu*; do
    [ -d "$d" ] || continue
    MU_DIRS+=( "$d" )
  done
fi

if [ ${#MU_DIRS[@]} -eq 0 ]; then
  echo "[ERR] No mu dirs found under $MU_ROOT (expect mu*)"
  exit 1
fi

echo "[i] detected mu dirs:"
printf '  - %s\n' "${MU_DIRS[@]}"

# ==========================
# mu ごとの外側ループ
# ==========================
for mu_dir in "${MU_DIRS[@]}"; do
  mu_base="$(basename "$mu_dir")"     # mu0.03
  MU="${mu_base#mu}"                 # 0.03

  OUTPUT_BASE="$mu_dir"              # ← 新しい入力元

  echo ""
  echo "======================================================="
  echo "[MU] $MU"
  echo "[OUTPUT_BASE] $OUTPUT_BASE"
  echo "======================================================="

  # preprocess
  echo "[RUN] preprocess all views under $OUTPUT_BASE (MU=$MU)"
  "$PYTHON_BIN" "$PREP_SCRIPT" \
    --mu "$MU" \
    --base-in "$OUTPUT_BASE" \
    --out-base "$RAW_BASE"

  # ==========================
  # view_id 列挙
  # ==========================
  VIEW_IDS=()
  for d in "$OUTPUT_BASE"/*; do
    [ -d "$d" ] || continue
    base="$(basename "$d")"

    # points / Process0000 はスキップ
    if [ "$base" = "points" ] || [ "$base" = "Process0000" ]; then
      continue
    fi

    # viewpoints が無いディレクトリもスキップ
    if [ ! -d "$d/viewpoints" ]; then
      continue
    fi

    VIEW_IDS+=( "$base" )
  done

  if [ ${#VIEW_IDS[@]} -eq 0 ]; then
    echo "[WARN] No valid view dirs under $OUTPUT_BASE (need viewpoints/) -> skip MU=$MU" >&2
    continue
  fi

  echo "[i] detected views for MU=$MU: ${VIEW_IDS[*]}"

  # ==========================
  # view ごとの内側ループ
  # ==========================
  for view_id in "${VIEW_IDS[@]}"; do
    NAME="mu${MU}_${view_id}"
    SRC_ROOT="${RAW_BASE}/${NAME}"

    if [ ! -d "$SRC_ROOT" ]; then
      echo "[WARN] raw dir not found for view_id=$view_id : $SRC_ROOT (skip)" >&2
      continue
    fi

    echo ""
    echo "[VIEW] $view_id"
    echo "  - SRC_ROOT = $SRC_ROOT"

    ## ---- (1) 通常モード ----
    OUT_DIR_NORMAL="${PROC_BASE}/${NAME}_${T0}to${T1}"
    REMOTE_PATH_NORMAL="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/SmokeRingPre/${NAME}_${T0}to${T1}"

    echo "  [NORMAL] OUT_DIR = $OUT_DIR_NORMAL"

    "$PYTHON_BIN" "$CONV_SCRIPT" \
      --src-root "$SRC_ROOT" \
      --out-dir  "$OUT_DIR_NORMAL" \
      --t0 "$T0" --t1 "$T1" \
      --sample-ts "$SAMPLE_TS" \
      --mu "$MU" \
      --time-step "$TIME_STEP"

    # echo "  [SCP NORMAL] $OUT_DIR_NORMAL -> ${REMOTE_USER}@${REMOTE_HOST}:$REMOTE_PATH_NORMAL"
    # scp -P "$REMOTE_PORT" -C -r "$OUT_DIR_NORMAL" "${REMOTE_USER}@${REMOTE_HOST}":"$REMOTE_PATH_NORMAL"

    ## ---- (2) eval_mode ----
    OUT_DIR_EVAL="${PROC_BASE}/${NAME}_sp${HALF_STEP}_${T0}to${T1}"
    REMOTE_PATH_EVAL="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/SmokeRingPre/${NAME}_sp${HALF_STEP}_${T0}to${T1}"

    echo "  [EVAL]   OUT_DIR = $OUT_DIR_EVAL"

    "$PYTHON_BIN" "$CONV_SCRIPT" \
      --src-root "$SRC_ROOT" \
      --out-dir  "$OUT_DIR_EVAL" \
      --t0 "$T0" --t1 "$T1" \
      --sample-ts "$SAMPLE_TS" \
      --mu "$MU" \
      --time-step "$TIME_STEP" \
      --eval_mode

    # echo "  [SCP EVAL]  $OUT_DIR_EVAL -> ${REMOTE_USER}@${REMOTE_HOST}:$REMOTE_PATH_EVAL"
    # scp -P "$REMOTE_PORT" -C -r "$OUT_DIR_EVAL" "${REMOTE_USER}@${REMOTE_HOST}":"$REMOTE_PATH_EVAL"
  done
  # ==========================
  # shared_points_cache を削除
  # ==========================
  SHARED_CACHE="/home/tomoya/Work/Data/tomoya/VisGaussian/SmokeRingPre/_shared_points_cache"

  if [ -d "$SHARED_CACHE" ]; then
      echo "[CLEANUP] removing shared points cache: $SHARED_CACHE"
      rm -rf "$SHARED_CACHE"
      echo "[CLEANUP] shared points cache removed."
  else
      echo "[CLEANUP] shared cache not found: $SHARED_CACHE (skip)"
  fi
done

echo ""
echo "[OK] all mu + views done."
