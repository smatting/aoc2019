{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
module Day5
where

import RIO
import qualified RIO.Text as T
import Control.Arrow ((>>>))
import Data.Bits

import qualified RIO.Vector.Unboxed as VU
import RIO.Vector.Unboxed ((!?))
import RIO.Vector.Unboxed.Partial ((//))

import Polysemy
import Polysemy.State

data Program
  = Program 
    { ip :: Int
    , memory :: VU.Vector Int
    , running :: Bool
    , input :: [Int]
    , output :: [Int]
    , debug :: Maybe Text
    }
    deriving (Show, Eq, Ord)

type MAddress = Int
type MValue = Int

data ParameterMode
  = PosMode
  | ImmMode
  deriving (Show, Eq, Ord)

parseProblem :: Text -> [Int]
parseProblem = T.split (== ',') >>> fmap (read . T.unpack)

opcode :: MValue -> Int
opcode n = n `mod` 100

pmode :: MValue -> Int -> ParameterMode
pmode n arg
  | n `div` (10 ^ (arg + 1)) `mod` 10 == 0 = PosMode
  | otherwise = ImmMode

moveIp ::
  Member (State Program) r =>
  Int ->
  Sem r ()
moveIp delta =
  modify (\s -> s {ip = ip s + delta})

computeOp3 ::
  Member (State Program) r =>
  (Int -> Int -> Int) ->
  MValue ->
  MValue ->
  MAddress ->
  Sem r ()
computeOp3 f a b madd = do
  modify (\s -> s {memory = memory s // [(madd, f a b)]})
  moveIp 4

parseParam ::
  Member (State Program) r =>
  ParameterMode ->
  MAddress ->
  Sem r (Maybe Int)
parseParam mode addr = do
  m <- gets memory
  return $
    m !? addr >>= \k ->
      case mode of
        PosMode -> m !? k
        ImmMode -> return k

assertSuccess ::
  Member (State Program) r =>
  Text ->
  Maybe (Sem r ()) ->
  Sem r ()
assertSuccess msg Nothing = haltError msg
assertSuccess _ (Just f) = f

haltError ::
  Member (State Program) r =>
  Text ->
  Sem r ()
haltError msg = do
  modify (\s -> s{ debug = Just msg})
  haltProgram

parseOp3 ::
  Member (State Program) r =>
  Int ->
  (Int -> Int -> Int -> Sem r ()) ->
  Sem r ()
parseOp3 k f = do
  i <- gets ip
  a <- parseParam (pmode k 1) (i + 1)
  b <- parseParam (pmode k 2) (i + 2)
  c <- parseParam (pmode k 3) (i + 3)
  assertSuccess "parseOp3 failed" $ f <$> a <*> b <*> c


-- readToAddr ::
--   Member (State Program) r =>
--   MAddress -> 
--   Sem r ()
-- readToAddr addr = do
--   inp <- gets input
--   assertSuccess "" $ do
--     k <- headMaybe inp

  

parseOp1 ::
  Member (State Program) r =>
  Int ->
  (Int -> Sem r ()) ->
  Sem r ()
parseOp1 k f = do
  i <- gets ip
  a <- parseParam (pmode k 1) (i + 1)
  assertSuccess "parseOp1 failed" $ f <$> a

haltProgram :: Member (State Program) r => Sem r ()
haltProgram = modify (\s -> s {running = False})

runOp ::
  Member (State Program) r =>
  Sem r ()
runOp = do
  i <- gets ip
  m <- gets memory
  flip (maybe (haltError "Unknown")) (m !? i) $ \k ->
    case opcode k of
      1 -> parseOp3 k (computeOp3 (+))
      2 -> parseOp3 k (computeOp3 (*))
      -- 3 -> parseOp1 k ()
      99 -> haltProgram

runUntilEnd :: Member (State Program) r => Sem r ()
runUntilEnd = do
  r <- gets running
  when r $ do
    runOp
    runUntilEnd

initProgram :: VU.Vector Int -> [Int] -> Program
initProgram intCode input =
  Program
    { ip = 0,
      memory = intCode,
      running = True,
      input = input,
      output = [],
      debug = Nothing
    }

runProgram :: Program -> Program
runProgram program = fst $
  runUntilEnd
    & runState program
    & run

main = do
  t <- readFileUtf8 "inputs/day2.txt"
  let intCode = VU.fromList (parseProblem t)
      intCode' = intCode // [(1, 12), (2, 2)]
  print $ runProgram (initProgram intCode' [1]) 

