#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../" || exit

month=$(date +%m)
day=$(date +%d)

if [[ $month != 12 ]]; then
  echo "Not december yet!"
  exit 1
fi

if [[ $day -gt 25 ]]; then
  echo "AoC is over!"
  exit 1
fi

cabal run fetch -- --day "$day"

cp -n src/AoC/Days/Day00.hs src/AoC/Days/Day"$day".hs
sed -i "s/Day00/Day$day/" src/AoC/Days/Day"$day".hs
echo "Created day file"

cp -n test/AoC/Days/Day00Spec.hs test/AoC/Days/Day"$day"Spec.hs
sed -i "s/Day00/Day$day/" test/AoC/Days/Day"$day"Spec.hs
sed -i "s/mkDay\ 0/mkDay\ $day/" test/AoC/Days/Day"$day"Spec.hs
echo "Created test file"

hpack
echo "Ran hpack"
