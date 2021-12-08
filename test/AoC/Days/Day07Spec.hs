module AoC.Days.Day07Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day07
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 07)
    parse
    (solveA, 336040)
    (solveB, 94813675)
