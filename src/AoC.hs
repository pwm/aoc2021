module AoC (solutions) where

import AoC.Core.Day
import AoC.Core.Solver
import AoC.Days.Day00 qualified as Day00
import AoC.Days.Day01 qualified as Day01
import AoC.Days.Day02 qualified as Day02
import AoC.Days.Day03 qualified as Day03
import AoC.Days.Day04 qualified as Day04
import AoC.Prelude

solutions :: Map Day (Day -> IO ())
solutions =
  solutionsMapper
    [ mkSolver Day00.parse Day00.solveA Day00.solveB,
      mkSolver Day01.parse Day01.solveA Day01.solveB,
      mkSolver Day02.parse Day02.solveA Day02.solveB,
      mkSolver Day03.parse Day03.solveA Day03.solveB,
      mkSolver Day04.parse Day04.solveA Day04.solveB
    ]
