#!/usr/bin/env bash
set -euo pipefail


# スクリプトに渡す引数
# 名前だけ指定すれば自動でSRC/OUT決定
EVAL_DATA_MODE=false
NAME="mu0.5_142"

########################################
# 1) 実行用の変数（ここを書き換える）
########################################
# Python 実行環境
PYTHON_BIN="${PYTHON_BIN:-python3}"

# 後処理（JSON→PLY）用スクリプトと出力場所
POSTPROC_SCRIPT="/home/tomoya/Work/GitHub/CUBEVis/Test/CNS-example/DrivAer_insitu_vis/for_4dgs/merge_isosurface_jsons.py"
POINTS_DIR="/data2/tomoya/CUBE/VisGaussian/raw/${NAME}/points"

# 実行するPythonスクリプト（フルパス推奨）
PY_SCRIPT="/home/tomoya/Work/GitHub/CUBEVis/Test/CNS-example/DrivAer_insitu_vis/for_4dgs/convert_from_insitu_to_4dgs.py"

# 時間パラメータ
T0=30
T1=130
SAMPLE_TS=30

# SRC/OUTの自動設定
SRC_ROOT="/data2/tomoya/CUBE/VisGaussian/raw/${NAME}"
OUT_DIR="/data2/tomoya/CUBE/VisGaussian/processed/${NAME}_${T0}to${T1}"
T0=30
T1=130
SAMPLE_TS=30

# 転送先（パスワードは手入力でOK）
REMOTE_USER="tomoyam"
REMOTE_HOST="10.34.33.218"
REMOTE_PORT=22
REMOTE_PATH="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/CUBEVis"   # リモートに作りたい/置きたい場所

########################################
# 2) 事前チェック
########################################
if [ ! -f "$POSTPROC_SCRIPT" ]; then
  echo "[ERR] CUBE後処理スクリプトが見つかりません: $POSTPROC_SCRIPT" >&2
  exit 1
fi

if ! command -v "$PYTHON_BIN" >/dev/null 2>&1; then
  echo "[ERR] Python 実行ファイルが見つかりません: $PYTHON_BIN" >&2
  exit 1
fi

if [ ! -f "$PY_SCRIPT" ]; then
  echo "[ERR] Python スクリプトが見つかりません: $PY_SCRIPT" >&2
  exit 1
fi

########################################
# 3) Python スクリプト実行
########################################

# まず整形
echo "[RUN] 整形: merge_isosurf -> points"
echo "      $PYTHON_BIN $POSTPROC_SCRIPT ${NAME}"
"$PYTHON_BIN" "$POSTPROC_SCRIPT" "${NAME}"

# 整形の結果（points）ができているか軽く確認
if [ ! -d "$POINTS_DIR" ]; then
  echo "[ERR] 整形後の points ディレクトリが見つかりません: $POINTS_DIR" >&2
  exit 1
fi

# 次に4dgsように変換
echo "[RUN] $PYTHON_BIN $PY_SCRIPT"
echo "      --src-root $SRC_ROOT"
echo "      --out-dir  $OUT_DIR"
echo "      --t0 $T0 --t1 $T1 --sample-ts $SAMPLE_TS"
"$PYTHON_BIN" "$PY_SCRIPT" \
  --src-root "$SRC_ROOT" \
  --out-dir  "$OUT_DIR" \
  --t0 "$T0" --t1 "$T1" --sample-ts "$SAMPLE_TS" --eval_mode "$EVAL_DATA_MODE"

# 出力確認
if [ ! -e "$OUT_DIR" ]; then
  echo "[ERR] 出力ディレクトリが見つかりませんでした: $OUT_DIR" >&2
  exit 1
fi

########################################
# 4) scpで転送（パスワード手入力）
########################################
# ディレクトリごと転送（-r で再帰、-C で圧縮）
echo "[SCP] $OUT_DIR -> ${REMOTE_USER}@${REMOTE_HOST}:$REMOTE_PATH"
scp -P "$REMOTE_PORT" -C -r "$OUT_DIR" "${REMOTE_USER}@${REMOTE_HOST}":"$REMOTE_PATH"

echo "[OK] すべて完了しました。"
