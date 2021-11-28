module AoC.Core.Solver where

import AoC.Core.Day
import AoC.Core.File
import AoC.Prelude

solveDay :: Map Day (Day -> IO ()) -> Day -> IO ()
solveDay solMap day = case solMap !? day of
  Nothing -> printError $ "Day " <> displayDay day <> " does not yet exists."
  Just solver -> solver day

mkSolver ::
  (Show a, Show b) =>
  (String -> Maybe i) ->
  (i -> a) ->
  (i -> b) ->
  (Day -> IO ())
mkSolver parser solverA solverB day = do
  file <- loadInputFile day
  case parser file of
    Nothing -> printError ("Cannot parse input file " <> displayDayFile day)
    Just input -> print (solverA input, solverB input)
