## (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ *Advent of Code 2021* (◕‿◕✿)

## Setup

    direnv allow .

## Use

### With Nix

Build:

    build

Solve:

    solve --day <day>

### With Cabal

Build:

    scripts/comp.sh

Solve:

    scripts/solve.sh <day>

## Tests

    scripts/test.sh

## Dev

Set session cookie for the fetcher:

    echo 'export AOC_SESSION=<my-aoc-session-cookie>' > .envrc.private

Create current day:

    scripts/mkday.sh

Create specific day:

    scripts/mkday.sh <day>
