#!/usr/bin/env bash
set -euo pipefail

# ====== 設定 ======
LOCAL_DIR="/home/tomoya/Work/Data/tomoya/VisGaussian/SmokeRingIso/processed"
REMOTE_DIR="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data/"
REMOTE_USER="tomoyam"
REMOTE_HOST="10.34.33.218"
REMOTE_PORT=22
# ==================

echo "[i] Copying:"
echo "    LOCAL : $LOCAL_DIR"
echo "    REMOTE: $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
echo ""

# リモート側 SmokeRing を作成（存在してもOK）
ssh -p "$REMOTE_PORT" "${REMOTE_USER}@${REMOTE_HOST}" \
  "mkdir -p \"$REMOTE_DIR\""

# processed を丸ごと 1 回でコピー（パスワード入力は 1 回）
scp -P "$REMOTE_PORT" -C -r "$LOCAL_DIR" \
    "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/"

echo ""
echo "[OK] processed -> SmokeRing へのコピーが完了しました。"
