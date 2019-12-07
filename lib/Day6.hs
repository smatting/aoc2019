module Day6

where

import RIO
import qualified RIO.Text as T
import RIO.List.Partial ((!!))

import qualified RIO.Map as Map

import Data.Tree

import Data.Maybe

parseProblem :: Text -> [(Text, Text)]
parseProblem t =
    T.lines t <&> \line ->
      let xs = T.split (== ')') line
       in (xs !! 0, xs !! 1)

childrenMap :: [(Text, Text)] -> Map.Map Text [Text]
childrenMap xs = foldl' f Map.empty xs
  where
    f m (k, v) = Map.insertWith (++) k [v] m

mkTree :: Map.Map Text [Text] -> Tree Text
mkTree m =
  unfoldTree 
    (\n -> (n, fromMaybe [] (Map.lookup n m)))
    "COM"

findNodes :: (a -> Bool) -> Tree a -> [[a]]
findNodes pred rootNode = fmap reverse $ go [] rootNode
  where
    go path (Node x []) = 
      [x : path | pred x]
    go path (Node x forest) = 
      concatMap (go (x:path)) forest

stripCommon :: Eq a => [a] -> [a] -> ([a], [a])
stripCommon l1@(x1:xs1) l2@(x2:xs2)
  | x1 == x2 = stripCommon xs1 xs2
  | otherwise = (l1, l2)
stripCommon l1 [] = (l1, [])
stripCommon [] l2 = ([], l2)

take2 :: [a] -> Maybe (a, a)
take2 [x1, x2] = Just (x1, x2) 
take2 _ = Nothing

solvePart1 :: Tree Text -> Int
solvePart1 tree =
  sum (zipWith (\depth ns -> depth * length ns) [0..] (levels tree))

solvePart2 :: Tree Text -> Int
solvePart2 tree =
  let (p1, p2) = fromJust $ take2 $ findNodes (\n -> n == "YOU" || n == "SAN") tree
      (p1', p2') = stripCommon p1 p2
   in length p1' + length p2' - 2

main = do
  -- t <- readFileUtf8 "inputs/day6.txt"
  t <- readFileUtf8 "inputs/day6.txt"
  let input = parseProblem t
      tree = mkTree (childrenMap input)
  print $ solvePart1 tree
  print $ solvePart2 tree

