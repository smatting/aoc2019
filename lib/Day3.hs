module Day3

where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Void
import RIO
import RIO.List (minimumMaybe)

type Parser = Parsec Void Text

data Direction = U | D | L | R
  deriving (Show, Eq, Ord)

type Point = (Int, Int)

data HLine = HLine Int Direction Int Int Int
  deriving (Show, Eq, Ord)

data VLine = VLine Int Direction Int Int Int
  deriving (Show, Eq, Ord)

dir :: Parser (Direction, Int)
dir =
  (,)
    <$> ( (char 'U' $> U)
            <|> (char 'D' $> D)
            <|> (char 'L' $> L)
            <|> (char 'R' $> R)
        )
    <*> decimal

input :: Parser ([(Direction, Int)], [(Direction, Int)])
input = do
  x1 <- dir `sepBy` string ","
  newline
  x2 <- dir `sepBy` string ","
  newline
  return (x1, x2)

parseInput t =
  fromMaybe
    (error "unhandled parser error")
    (parseMaybe input t)

translate :: (Direction, Int) -> Point -> Point
translate (U, d) (x, y) = (x, y - d)
translate (D, d) (x, y) = (x, y + d)
translate (L, d) (x, y) = (x - d, y)
translate (R, d) (x, y) = (x + d, y)

intersect :: HLine -> VLine -> Maybe Point
intersect (HLine _ _ y xmin xmax) (VLine _ _ x ymin ymax)
  | xmin <= x && x <= xmax && ymin <= y && y <= ymax = Just (x, y)
  | otherwise = Nothing

hlineDist :: HLine -> Int -> Int
hlineDist (HLine d R _ xmin _) x = d + (x - xmin)
hlineDist (HLine d L _ _ xmax) x = d + (xmax - x)
hlineDist _                    _ = 0

vlineDist :: VLine -> Int -> Int
vlineDist (VLine d U _ _ ymax) y = d + (ymax - y)
vlineDist (VLine d D _ ymin _) y = d + (y - ymin)
vlineDist _                    _ = 0

wline :: Int -> Point -> (Direction, Int) -> (Int, Point, Either HLine VLine)
wline dist p@(x, y) dp@(dir, delta) =
  let p'@(x', y') = translate dp p
      dist' = dist + delta
   in case dir of
        U -> (dist', p', Right (VLine dist U x y' y))
        D -> (dist', p', Right (VLine dist D x y y'))
        L -> (dist', p', Left (HLine dist L y x' x))
        R -> (dist', p', Left (HLine dist R y x x'))

wlines' :: Int -> Point -> [(Direction, Int)] -> [Either HLine VLine]
wlines' _ _ [] = []
wlines' d p (dp:dps) =
  let (d', p', e') = wline d p dp
   in e' : wlines' d' p' dps

wlines = wlines' 0 (0, 0)

sortLines :: [Either HLine VLine] -> ([HLine], [VLine])
sortLines = foldr f ([], [])
  where
    f (Left h) (hs, vs) = (h:hs, vs)
    f (Right v) (hs, vs) = (hs, v:vs)

intersections :: ([HLine], [VLine]) -> ([HLine], [VLine]) -> [Point]
intersections (hlines1, vlines1) (hlines2, vlines2) =
  f hlines1 vlines2 ++ f hlines2 vlines1
    where
      f hs vs = catMaybes [h `intersect` v | h <- hs, v <- vs]

manhattan :: Point -> Int
manhattan (x, y) = abs x + abs y

solvePart1 :: ([(Direction, Int)], [(Direction, Int)]) -> Maybe Int
solvePart1 input =
  minimumMaybe
    ( fmap
        manhattan
        ( intersections
            (sortLines (wlines (fst input)))
            (sortLines (wlines (snd input)))
        )
    )

intersectDist :: HLine -> VLine -> Maybe (Int, Point)
intersectDist h@(HLine _ _ y xmin xmax) v@(VLine _ _ x ymin ymax)
  | xmin <= x && x <= xmax && ymin <= y && y <= ymax =
      Just (hlineDist h x + vlineDist v y, (x, y))
  | otherwise = Nothing


intersectDists :: ([HLine], [VLine]) -> ([HLine], [VLine]) -> [(Int, Point)]
intersectDists (hlines1, vlines1) (hlines2, vlines2) =
  f hlines1 vlines2 ++ f hlines2 vlines1
    where
      f hs vs = catMaybes [h `intersectDist` v | h <- hs, v <- vs]

solvePart2 :: ([(Direction, Int)], [(Direction, Int)]) -> Maybe Int
solvePart2 input =
  minimumMaybe
    ( fmap
        fst
        ( intersectDists
            (sortLines (wlines (fst input)))
            (sortLines (wlines (snd input)))
        )
    )

main = do
  t <- readFileUtf8 "inputs/day3.txt"
  let input = parseInput t
  print $ solvePart1 input
  print $ solvePart2 input
