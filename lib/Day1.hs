{-|
Description: https://adventofcode.com/2019/day/1
-}

module Day1

where

import RIO
import qualified RIO.Text as T

parseProblem :: Text -> [Int]
parseProblem = T.lines >>> fmap (read . T.unpack)

fuel :: Int -> Int
fuel x = x `div` 3 - 2

solvePart1 :: [Int] -> Int
solvePart1 xs = sum (fmap fuel xs)

fuelSum' :: Int -> Int -> Int
fuelSum' acc x =
  let k = fuel x
   in if k > 0 then fuelSum' (acc + k) k else acc

fuelSum :: Int -> Int
fuelSum = fuelSum' 0

solvePart2 :: [Int] -> Int
solvePart2 = sum . fmap fuelSum

main = do
  t <- readFileUtf8 "inputs/day1.txt"
  print $ solvePart1 (parseProblem t)
  print $ solvePart2 (parseProblem t)
