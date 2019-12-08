module Main (main) where

import Options.Applicative
import Data.Semigroup ((<>))

import RIO


import qualified Lib as Lib

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7

day :: Int -> IO ()
day 1 = Day1.main
day 2 = Day2.main
day 3 = Day3.main
day 4 = Day4.main
day 5 = Day5.main
day 6 = Day6.main
day 7 = Day7.main

args :: Parser (IO ())
args = day <$> argument auto (metavar "DAY")

main :: IO ()
main = join $ execParser opts
  where
    opts =
      info
        (args <**> helper)
        ( fullDesc
            <> progDesc "Run day of advent of code 2019"
        )
