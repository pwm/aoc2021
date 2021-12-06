module AoC.Days.Day06Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day06
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 06)
    parse
    (solveA, 361169)
    (solveB, 1634946868992)
