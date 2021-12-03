module AoC.Days.Day03Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day03
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 03)
    parse
    (solveA, 4174964)
    (solveB, 4474944)
