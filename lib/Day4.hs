module Day4
where
  
import RIO
import RIO.List (group)

type BigTuple = (Int, Int, Int, Int, Int, Int)

genTuples :: (Int -> Int -> Int -> Int -> Int -> Int -> Bool) -> [BigTuple]
genTuples pred = do
  x1 <- [0 .. 9]
  x2 <- [x1 .. 9]
  x3 <- [x2 .. 9]
  x4 <- [x3 .. 9]
  x5 <- [x4 .. 9]
  x6 <- [x5 .. 9]
  guard $ pred x1 x2 x3 x4 x5 x6
  return (x1, x2, x3, x4, x5, x6)

digits :: Int -> (Int, Int, Int, Int, Int, Int)
digits n =
  ( g n 1000000,
    g n 100000,
    g n 10000,
    g n 1000,
    g n 100,
    g n 10
  )
  where
    g :: Int -> Int -> Int
    g n k = (n `mod` k) `quot` (k `quot` 10)

filterRange :: Int -> Int -> [BigTuple] -> [BigTuple]
filterRange xmin xmax xs =
  let dmin = digits xmin
      dmax = digits xmax
      p x = dmin <= x && x <= dmax
  in filter p xs

pred1 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pred1 x1 x2 x3 x4 x5 x6 =
  x1 == x2 || x2 == x3 || x3 == x4 || x4 == x5 || x5 == x6

pred2 :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
pred2 x1 x2 x3 x4 x5 x6 =
  elem 2 $ fmap length (group [x1, x2, x3, x4, x5, x6])

solvePart1 :: Int -> Int -> Int
solvePart1 xmin xmax =
  length (filterRange xmin xmax (genTuples pred1))

solvePart2 :: Int -> Int -> Int
solvePart2 xmin xmax =
  length (filterRange xmin xmax (genTuples pred2))

main = do
  print $ solvePart1 134564 585159
  print $ solvePart2 134564 585159
