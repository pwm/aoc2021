module AoC.Days.Day13Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day13
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 13)
    parse
    (solveA, 850)
    (solveB, "AHGCPGAU")
