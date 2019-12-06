{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, TypeApplications #-}
module Day5
where

import Prelude hiding ((!!))

import RIO
import RIO.List
import qualified RIO.Text as T
import Control.Arrow ((>>>))

import qualified RIO.Vector.Unboxed as VU
import RIO.Vector.Unboxed ((!?))
import RIO.Vector.Unboxed.Partial ((//))

import Polysemy
import Polysemy.State
import Polysemy.Error

data Program
  = Program 
    { ip :: Int
    , memory :: VU.Vector Int
    , running :: Bool
    , input :: [Int]
    , output :: [Int]
    , debug :: Maybe ErrorMsg
    }
    deriving (Show, Eq, Ord)

showt :: Show a => a -> Text
showt = T.pack . show

data ErrorMsg = ErrorMsg Text
  deriving (Show, Eq, Ord)

type MAddress = Int
type MValue = Int

data ParameterMode
  = PosMode
  | ImmMode
  deriving (Show, Eq, Ord)

type Parameter = (ParameterMode, Int)

assertIndex ::
  Member (Error ErrorMsg) r =>
  VU.Vector Int ->
  Int ->
  Sem r ()
assertIndex v i
  | 0 <= i && i < VU.length v = return ()
  | otherwise = throw (ErrorMsg ("Intcode: Memory access error @ " <> showt i))

writeMemory ::
  Members [State Program, Error ErrorMsg] r =>
  MAddress ->
  MValue ->
  Sem r ()
writeMemory addr val = do
  m <- gets memory
  assertIndex m addr
  modify (\s -> s {memory = m // [(addr, val)]})

assertJust ::
  Member (Error ErrorMsg) r =>
  Text ->
  Maybe a ->
  Sem r a
assertJust msg Nothing = throw (ErrorMsg msg)
assertJust _ (Just x) = return x

(!!) :: Member (Error ErrorMsg) r => VU.Vector Int -> Int -> Sem r Int
(!!) v i = assertJust ("Intcode: Memory access error @ " <> showt i) (v !? i)

readMemory ::
  Members [State Program, Error ErrorMsg] r =>
  MAddress ->
  Sem r MValue
readMemory addr = do
  m <- gets memory
  m !! addr

opcode :: MValue -> Int
opcode n = n `mod` 100

pmode :: MValue -> Int -> ParameterMode
pmode n arg
  | n `div` (10 ^ (arg + 1)) `mod` 10 == 0 = PosMode
  | otherwise = ImmMode

moveIpBy ::
  Member (State Program) r =>
  Int ->
  Sem r ()
moveIpBy delta =
  modify (\s -> s {ip = ip s + delta})

moveIpTo ::
  Member (State Program) r =>
  Int ->
  Sem r ()
moveIpTo ip' =
  modify (\s -> s {ip = ip'})

--------------------------------------------------------------------------------

parseOp1 ::
  Members [State Program, Error ErrorMsg] r =>
  Int ->
  (Parameter -> Sem r ()) ->
  Sem r ()
parseOp1 k f = do
  i <- gets ip
  join $ f <$> ((pmode k 1, ) <$> readMemory (i + 1))

parseOp2 ::
  Members [State Program, Error ErrorMsg] r =>
  Int ->
  (Parameter -> Parameter -> Sem r ()) ->
  Sem r ()
parseOp2 k f = do
  i <- gets ip
  join $
    f <$> ((pmode k 1, ) <$> readMemory (i + 1))
      <*> ((pmode k 2, ) <$> readMemory (i + 2))

parseOp3 ::
  Members [State Program, Error ErrorMsg] r =>
  Int ->
  (Parameter -> Parameter -> Parameter -> Sem r ()) ->
  Sem r ()
parseOp3 k f = do
  i <- gets ip
  join $
    f <$> ((pmode k 1, ) <$> readMemory (i + 1))
      <*> ((pmode k 2, ) <$> readMemory (i + 2))
      <*> ((pmode k 3, ) <$> readMemory (i + 3))

readParam ::
  Members [State Program, Error ErrorMsg] r =>
  Parameter ->
  Sem r Int
readParam (ImmMode, val) = return val
readParam (PosMode, addr) = readMemory addr

withPosParam ::
  Members [State Program, Error ErrorMsg] r =>
  Parameter ->
  (MAddress -> Sem r ()) ->
  Sem r ()
withPosParam (PosMode, addr) f = f addr
withPosParam (ImmMode, _) _ = haltWithError (ErrorMsg "Expected positional parameter")

opCombine2Write1 ::
  Members [State Program, Error ErrorMsg] r =>
  (Int -> Int -> Int) ->
  Parameter ->
  Parameter ->
  Parameter ->
  Sem r ()
opCombine2Write1 f p1 p2 p3 = do
  withPosParam p3 $ \addr -> do
    k <- f <$> readParam p1 <*> readParam p2
    writeMemory addr k
    moveIpBy 4

haltProgram :: Member (State Program) r => Sem r ()
haltProgram = modify (\s -> s {running = False})

opReadInput ::
  Members [State Program, Error ErrorMsg] r =>
  Parameter ->
  Sem r ()
opReadInput p =
  withPosParam p $ \addr -> do
    inp <- gets input
    (x, input') <- assertJust "Intcode: End of input error" $ (,) <$> headMaybe inp <*> tailMaybe inp
    writeMemory addr x
    modify (\s -> s {input = input'})
    moveIpBy 2

opWriteOutput ::
  Members [State Program, Error ErrorMsg] r =>
  Parameter ->
  Sem r ()
opWriteOutput p = do
  v <- readParam p
  modify (\s -> s {output = v:output s})
  moveIpBy 2

opJumpCondition1 ::
  Members [State Program, Error ErrorMsg] r =>
  (Int -> Bool) ->
  Parameter ->
  Parameter ->
  Sem r ()
opJumpCondition1 pred p1 p2 = do
  x1 <- readParam p1
  x2 <- readParam p2
  if pred x1
     then moveIpTo x2
     else moveIpBy 3

runOp ::
  Members [State Program, Error ErrorMsg] r =>
  Sem r ()
runOp = do
  i <- gets ip
  m <- gets memory
  k <- m !! i
  case opcode k of
    1 -> parseOp3 k (opCombine2Write1 (+))
    2 -> parseOp3 k (opCombine2Write1 (*))
    3 -> parseOp1 k opReadInput
    4 -> parseOp1 k opWriteOutput
    5 -> parseOp2 k (opJumpCondition1 (/= 0))
    6 -> parseOp2 k (opJumpCondition1 (== 0))
    7 -> parseOp3 k (opCombine2Write1 (\a b -> if a < b then 1 else 0))
    8 -> parseOp3 k (opCombine2Write1 (\a b -> if a == b then 1 else 0))
    99 -> haltProgram
    x -> haltWithError (ErrorMsg ("Unknown opcode: " <> showt x))

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

haltWithError ::
  Members '[State Program] r =>
  ErrorMsg ->
  Sem r ()
haltWithError msg = do
  modify (\s -> s {debug = Just msg} )
  haltProgram

runUntilEnd ::
  Members [State Program, Error ErrorMsg] r =>
  Sem r ()
runUntilEnd = do
  r <- gets running
  when r $ do
    runOp
    runUntilEnd

runDebug ::
  Members [State Program, Error ErrorMsg, Embed IO] r =>
  Sem r ()
runDebug = do
  prg <- get @Program
  embed (print prg)
  embed getLine
  r <- gets running
  when r $ do
    runOp
    runDebug

runProgram :: Program -> Program
runProgram program = fst $
  runUntilEnd
    & runError
    & fmap (either haltWithError return)
    & join
    & runState program
    & run

runProgramDebug :: Program -> IO ()
runProgramDebug program =
  runDebug
    & runError
    & fmap (either haltWithError return)
    & join
    & runState program
    & runM
    & (print =<<) . fmap fst

solvePart1 :: [Int] -> Maybe Int
solvePart1 intcode =
  let prg = runProgram (initProgram (VU.fromList intcode) [1])
   in headMaybe $ output prg

solvePart2 :: [Int] -> Maybe Int
solvePart2 intcode =
  let prg = runProgram (initProgram (VU.fromList intcode) [5])
   in headMaybe $ output prg

parseProblem :: Text -> [Int]
parseProblem = T.split (== ',') >>> fmap (read . T.unpack)

main = do
  t <- readFileUtf8 "inputs/day5.txt"
  let intcode = (parseProblem t)
  print $ solvePart1 intcode
  print $ solvePart2 intcode
