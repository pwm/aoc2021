module AoC.Core.File where

import AoC.Core.Day
import AoC.Prelude

loadInputFile :: Day -> IO String
loadInputFile day = do
  f <- getDataFileName $ "input/" <> displayDayFile day
  readFile f
