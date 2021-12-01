module AoC.Lib.Load where

import AoC.Core.Day
import AoC.Core.File
import AoC.Prelude

load :: Int -> IO String
load d = case mkDay d of
  Left s -> pure s
  Right day -> loadInputFile day
