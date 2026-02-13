#!/usr/bin/env bash
set -euo pipefail

LOCAL_DIR="/home/tomoya/Work/Data/tomoya/VisGaussian/SmokeRingPre/processed"
REMOTE_DIR="/home/tomoyam/Work/tomoya/VisGaussian/ForTimestep/data"
REMOTE_USER="tomoyam"
REMOTE_HOST="10.34.33.218"
REMOTE_PORT=22

echo "[i] Copying entire processed directory:"
echo "    LOCAL : $LOCAL_DIR"
echo "    REMOTE: $REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR"
echo ""

# mkdir（1回だけ）
ssh -p "$REMOTE_PORT" "${REMOTE_USER}@${REMOTE_HOST}" "mkdir -p '$REMOTE_DIR'"

# processed を丸ごとコピー（★1回の scp）
scp -P "$REMOTE_PORT" -C -r "$LOCAL_DIR" \
  "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_DIR}/"

echo ""
echo "[OK] processed directory copied successfully."
