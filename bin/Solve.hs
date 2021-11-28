module Main (main) where

import AoC
import AoC.Core.ArgParser
import AoC.Core.Solver
import AoC.Prelude

main :: IO ()
main = execArgParser opts >>= either printError (solveDay solutions)
