module AoC (solutions) where

import AoC.Core.Day
import AoC.Core.Solver
import AoC.Days.Day00 qualified as Day00
import AoC.Days.Day01 qualified as Day01
import AoC.Prelude

solutions :: Map Day (Day -> IO ())
solutions =
  solutionsMapper
    [ mkSolver Day00.parse Day00.solveA Day00.solveB,
      mkSolver Day01.parse Day01.solveA Day01.solveB
    ]
