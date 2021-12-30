module AoC.Days.Day25Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day25
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 25)
    parse
    (solveA, 518)
    (solveB, ())
