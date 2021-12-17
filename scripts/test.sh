#!/usr/bin/env bash
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")/../" || exit

test_options=""
if [[ $# == 1 ]]; then
  test_options="--test-option=--match --test-option=$1"
else
  for i in "$@"; do
    test_options="$test_options --test-option=$i"
  done
fi

# shellcheck disable=SC2086
cabal --ghc-options -Wwarn test --test-show-details=direct $test_options
