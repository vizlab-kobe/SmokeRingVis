#!/usr/bin/env bash
set -euo pipefail

# ====== 好みのデフォルト（環境変数で上書き可）======
MU_DEFAULT="0.02"
VIEWPOINT_DEFAULT="182"

# 呼び出し側から MU / VIEWPOINT が来ていればそれを優先
MU="${MU:-$MU_DEFAULT}"
VIEWPOINT="${VIEWPOINT:-$VIEWPOINT_DEFAULT}"

echo "[i] Using MU=${MU}, VIEWPOINT=${VIEWPOINT}"

# ====== ビルド & 実行フロー ======

./clean.sh
./make.sh
./run.sh

# for_4dgs のランナーに MU/VIEWPOINT を渡して実行
# ここで export して子プロセスに引き継ぐ
export MU VIEWPOINT
./for_4dgs/run_convert_and_copy.sh
