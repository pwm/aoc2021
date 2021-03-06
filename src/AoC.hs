module AoC (solutions) where

import AoC.Core.Day
import AoC.Core.Solver
import AoC.Days.Day00 qualified as Day00
import AoC.Days.Day01 qualified as Day01
import AoC.Days.Day02 qualified as Day02
import AoC.Days.Day03 qualified as Day03
import AoC.Days.Day04 qualified as Day04
import AoC.Days.Day05 qualified as Day05
import AoC.Days.Day06 qualified as Day06
import AoC.Days.Day07 qualified as Day07
import AoC.Days.Day08 qualified as Day08
import AoC.Days.Day09 qualified as Day09
import AoC.Days.Day10 qualified as Day10
import AoC.Days.Day11 qualified as Day11
import AoC.Days.Day12 qualified as Day12
import AoC.Days.Day13 qualified as Day13
import AoC.Days.Day14 qualified as Day14
import AoC.Days.Day15 qualified as Day15
import AoC.Days.Day16 qualified as Day16
import AoC.Days.Day17 qualified as Day17
import AoC.Days.Day18 qualified as Day18
import AoC.Days.Day19 qualified as Day19
import AoC.Days.Day20 qualified as Day20
import AoC.Days.Day21 qualified as Day21
import AoC.Days.Day22 qualified as Day22
import AoC.Days.Day23 qualified as Day23
import AoC.Days.Day24 qualified as Day24
import AoC.Days.Day25 qualified as Day25
import AoC.Prelude

solutions :: Map Day (Day -> IO ())
solutions =
  solutionsMapper
    [ mkSolver Day00.parse Day00.solveA Day00.solveB,
      mkSolver Day01.parse Day01.solveA Day01.solveB,
      mkSolver Day02.parse Day02.solveA Day02.solveB,
      mkSolver Day03.parse Day03.solveA Day03.solveB,
      mkSolver Day04.parse Day04.solveA Day04.solveB,
      mkSolver Day05.parse Day05.solveA Day05.solveB,
      mkSolver Day06.parse Day06.solveA Day06.solveB,
      mkSolver Day07.parse Day07.solveA Day07.solveB,
      mkSolver Day08.parse Day08.solveA Day08.solveB,
      mkSolver Day09.parse Day09.solveA Day09.solveB,
      mkSolver Day10.parse Day10.solveA Day10.solveB,
      mkSolver Day11.parse Day11.solveA Day11.solveB,
      mkSolver Day12.parse Day12.solveA Day12.solveB,
      mkSolver Day13.parse Day13.solveA Day13.solveB,
      mkSolver Day14.parse Day14.solveA Day14.solveB,
      mkSolver Day15.parse Day15.solveA Day15.solveB,
      mkSolver Day16.parse Day16.solveA Day16.solveB,
      mkSolver Day17.parse Day17.solveA Day17.solveB,
      mkSolver Day18.parse Day18.solveA Day18.solveB,
      mkSolver Day19.parse Day19.solveA Day19.solveB,
      mkSolver Day20.parse Day20.solveA Day20.solveB,
      mkSolver Day21.parse Day21.solveA Day21.solveB,
      mkSolver Day22.parse Day22.solveA Day22.solveB,
      mkSolver Day23.parse Day23.solveA Day23.solveB,
      mkSolver Day24.parse Day24.solveA Day24.solveB,
      mkSolver Day25.parse Day25.solveA Day25.solveB
    ]
