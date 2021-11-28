module Main (main) where

import AoC.Core.ArgParser
import AoC.Core.Fetcher
import AoC.Prelude

main :: IO ()
main = execArgParser opts >>= either printError (fetchDay "2021")
