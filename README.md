## (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ *Advent of Code 2021* (◕‿◕✿)

## Setup

    direnv allow .

## Use

### With Cabal

    scripts/comp.sh
    scripts/solve.sh <day>

### With Nix

    build
    solve --day <day>

## Test

    scripts/test.sh
    scripts/test.sh <day>

## Dev

Set session cookie for the fetcher:

    echo 'export AOC_SESSION=<my-aoc-session-cookie>' > .envrc.private

Create current day:

    scripts/mkday.sh
    scripts/mkday.sh <day>
