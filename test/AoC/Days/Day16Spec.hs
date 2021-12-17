module AoC.Days.Day16Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day16
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 16)
    parse
    (solveA, 957)
    (solveB, 744953223228)
