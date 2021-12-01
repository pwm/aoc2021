module AoC.Core.ArgParser where

import AoC.Core.Day
import Options.Applicative
import AoC.Prelude

execArgParser :: ParserInfo a -> IO a
execArgParser = execParser

opts :: ParserInfo (Either String Day)
opts =
  info
    (parseDay <**> helper)
    ( fullDesc
        <> progDesc "Advent of Code"
        <> header "Run solutions to this year's AoC"
    )
  where
    parseDay :: Parser (Either String Day)
    parseDay =
      mkDay
        <$> option
          auto
          ( long "day"
              <> short 'd'
              <> metavar "DAY"
              <> help "Which day to solve"
          )
