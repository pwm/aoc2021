## (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ *Advent of Code 2021* (◕‿◕✿)

## Use

Build env:

    direnv allow .

Set session cookie for the fetcher:

    echo 'export AOC_SESSION=<my-aoc-session-cookie>' > .envrc.private

Build app:

    scripts/comp.sh

Solve specific day:

    scripts/solve.sh <day>

Run tests:

    scripts/test.sh

## Dev

Create current day:

    scripts/mkday.sh

Create specific day:

    scripts/mkday.sh <day>

