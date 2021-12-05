#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../" || exit

if [[ $# -ne 1 ]]; then
  echo "Need day."
  exit 1
fi
day=$1

cabal run solve -- --day "$day"
