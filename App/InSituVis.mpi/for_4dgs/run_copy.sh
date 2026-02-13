#!/usr/bin/env bash
set -euo pipefail

# === 手動設定値（必要ならコメント解除して使う）========
MU_MANUAL="0.04"
VIEWPOINT_MANUAL="143"

EVAL_DATA_MODE=true

# =========================================================

# 1) 手動設定 > 環境変数 > 最終エラー の優先順位
MU="${MU_MANUAL:-${MU:-}}"
VIEWPOINT="${VIEWPOINT_MANUAL:-${VIEWPOINT:-}}"
NAME="mu${MU}_${VIEWPOINT}"


# Python 実行環境
PYTHON_BIN="${PYTHON_BIN:-python3}"

# スクリプト
PY_SCRIPT="/home/tomoya/Work/GitHub/SmokeRingVis/App/InSituVis.mpi/for_4dgs/convert_from_insitu_to_4dgs.py"

# 時間パラメータ
T0=0
T1=3000
SAMPLE_TS=100
TIME_STEP=200   # 10→100 に対応（eval は自動でその中点 = 50 を採用）。不要ならコメントアウトOK

# 入出力（固定規約に合わせる）
SRC_ROOT="/data2/tomoya/VisGaussian/SmokeRing/raw/${NAME}"

# TIME_STEP//2 (整数割り算)
HALF_STEP=$(( TIME_STEP / 2 ))

if [ "$EVAL_DATA_MODE" = true ]; then
    OUT_DIR="/data2/tomoya/VisGaussian/SmokeRing/processed/${NAME}_sp${HALF_STEP}_${T0}to${T1}"
else
    OUT_DIR="/data2/tomoya/VisGaussian/SmokeRing/processed/${NAME}_${T0}to${T1}"
fi

# 転送先
REMOTE_USER="tomoyam"
REMOTE_HOST="10.34.33.218"
REMOTE_PORT=22

if [ "$EVAL_DATA_MODE" = true ]; then
    REMOTE_PATH="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/SmokeRing/${NAME}_sp${HALF_STEP}_${T0}to${T1}"
else
    REMOTE_PATH="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/SmokeRing/${NAME}_${T0}to${T1}"
fi

# ===== 事前チェック =====
if ! command -v "$PYTHON_BIN" >/dev/null 2>&1; then
  echo "[ERR] Python 実行ファイルが見つかりません: $PYTHON_BIN" >&2
  exit 1
fi

if [ ! -f "$PY_SCRIPT" ]; then
  echo "[ERR] 変換スクリプトが見つかりません: $PY_SCRIPT" >&2
  exit 1
fi


# ===== 1) MultipleView 形式に変換 =====
echo "[RUN] ${PY_SCRIPT}"
echo "      --src-root $SRC_ROOT"
echo "      --out-dir  $OUT_DIR"
echo "      --t0 $T0 --t1 $T1 --sample-ts $SAMPLE_TS --mu $MU --time-step $TIME_STEP"
echo "      eval_mode = $EVAL_DATA_MODE"


# eval_modeフラグ（EVAL_DATA_MODE=true のときだけ ON）
EV_FLAG=""
if [ "$EVAL_DATA_MODE" = true ]; then
    EV_FLAG="--eval_mode"
fi

"$PYTHON_BIN" "$PY_SCRIPT" \
  --src-root "$SRC_ROOT" \
  --out-dir  "$OUT_DIR" \
  --t0 "$T0" --t1 "$T1" \
  --sample-ts "$SAMPLE_TS" \
  --mu "$MU" \
  --time-step "$TIME_STEP" \
  $EV_FLAG

# ===== 2) 出力確認 =====
if [ ! -e "$OUT_DIR" ]; then
  echo "[ERR] 出力ディレクトリが見つかりませんでした: $OUT_DIR" >&2
  exit 1
fi

# ===== 3) 転送 =====
echo "[SCP] $OUT_DIR -> ${REMOTE_USER}@${REMOTE_HOST}:$REMOTE_PATH"
scp -P "$REMOTE_PORT" -C -r "$OUT_DIR" "${REMOTE_USER}@${REMOTE_HOST}":"$REMOTE_PATH"

echo "[OK] すべて完了しました。"
