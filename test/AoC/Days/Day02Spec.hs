module AoC.Days.Day02Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day02
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 02)
    parse
    (solveA, 1660158)
    (solveB, 1604592846)
