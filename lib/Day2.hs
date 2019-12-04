{-|
Description: https://adventofcode.com/2019/day/2
-}

{-# language LambdaCase #-}

module Day2
where

import RIO
import qualified RIO.Text as T
import Control.Arrow ((>>>))
import qualified RIO.Vector.Unboxed as VU
import RIO.Vector.Unboxed ((!?))
import RIO.Vector.Unboxed.Partial ((//))

parseProblem :: Text -> [Int]
parseProblem = T.split (== ',') >>> fmap (read . T.unpack)

runProgram :: Int -> VU.Vector Int -> VU.Vector Int
runProgram k v =
  flip (maybe v) (v !? k) $ \case
    1 -> runProgram (k + 4) (f (+))
    2 -> runProgram (k + 4) (f (*))
    99 -> v
    _ -> error "unexpected opcode"
  where
    f op = fromMaybe v $ do
      i1 <- v !? (k + 1) 
      i2 <- v !? (k + 2) 
      i3 <- v !? (k + 3)
      result <- op <$> v !? i1 <*> v !? i2
      return (v // [(i3, result)])

runWithInput :: Int -> Int -> VU.Vector Int -> Int
runWithInput a b v = fromMaybe 0 $ do
  let v2 = v // [(1, a), (2, b)]
  runProgram 0 v2 !? 0

solvePart1 :: [Int] -> Int
solvePart1 xs = runWithInput 12 2 (VU.fromList xs)

solvePart2 :: [Int] -> Maybe Int
solvePart2 xs =
  let v = VU.fromList xs
   in listToMaybe $ do
    noun <- [0..99]
    verb <- [0..99]
    guard (runWithInput noun verb v == 19690720)
    return $ 100 * noun + verb

main = do
  t <- readFileUtf8 "inputs/day2.txt"
  print $ solvePart1 (parseProblem t)
  print $ solvePart2 (parseProblem t)
