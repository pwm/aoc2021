## (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ *Advent of Code 2021* (◕‿◕✿)

Build env:

    $ direnv allow .

Build app:

    $ hpack && cabal build

Set session cookie for the fetcher:

    $ echo 'export AOC_SESSION=<my-aoc-2020-session-cookie>' > .envrc.private

Create day:

    $ scripts/mkday.sh

Solve day:

    $ cabal run solve -- --day <day>

