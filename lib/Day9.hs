{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, TypeApplications #-}
module Day9
where

import Prelude hiding ((!!))

import RIO hiding (catch)
import RIO.List
import qualified RIO.Text as T
import Control.Arrow ((>>>))

import qualified RIO.Map as Map

import Polysemy
import Polysemy.State
import Polysemy.Error

type Memory = Map Int Int

data Program
  = Program 
    { ip :: Int
    , memory :: Memory
    , running :: Bool
    , relativeBase :: Int
    , input :: [Int]
    , output :: [Int]
    , err :: Maybe IntError
    }
    deriving (Show, Eq, Ord)

showt :: Show a => a -> Text
showt = T.pack . show

data IntErrorType =
    MemoryAccessError
  | InputIOError
  | ProgramError
  | GenericError
  deriving (Show, Eq, Ord)

data IntError = IntError IntErrorType Text
  deriving (Show, Eq, Ord)

type MAddress = Int
type MValue = Int

data ParameterMode
  = PosMode
  | ImmMode
  | RelMode
  deriving (Show, Eq, Ord)

type Parameter = (ParameterMode, Int)

assertIndex ::
  Member (Error IntError) r =>
  Int ->
  Sem r ()
assertIndex i
  | 0 <= i = return ()
  | otherwise = throw (IntError MemoryAccessError ("Intcode: Memory access error @ " <> showt i))

writeMemory ::
  Members [State Program, Error IntError] r =>
  MAddress ->
  MValue ->
  Sem r ()
writeMemory addr val = do
  m <- gets memory
  assertIndex addr
  modify (\s -> s {memory = Map.insert addr val m})

assertJust ::
  Member (Error IntError) r =>
  Text ->
  Maybe a ->
  Sem r a
assertJust msg Nothing = throw (IntError GenericError msg)
assertJust _ (Just x) = return x

(!!) :: Member (Error IntError) r => Memory -> Int -> Sem r Int
(!!) m i = return $ fromMaybe 0 (Map.lookup i m)

readMemory ::
  Members [State Program, Error IntError] r =>
  MAddress ->
  Sem r MValue
readMemory addr = do
  m <- gets memory
  m !! addr

opcode :: MValue -> Int
opcode n = n `mod` 100

pmode :: MValue -> Int -> ParameterMode
pmode n arg =
  case (n `div` (10 ^ (arg + 1)) `mod` 10) of 
    0 -> PosMode
    1 -> ImmMode
    _ -> RelMode

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
  Members [State Program, Error IntError] r =>
  Int ->
  (Parameter -> Sem r ()) ->
  Sem r ()
parseOp1 k f = do
  i <- gets ip
  join $ f <$> ((pmode k 1, ) <$> readMemory (i + 1))

parseOp2 ::
  Members [State Program, Error IntError] r =>
  Int ->
  (Parameter -> Parameter -> Sem r ()) ->
  Sem r ()
parseOp2 k f = do
  i <- gets ip
  join $
    f <$> ((pmode k 1, ) <$> readMemory (i + 1))
      <*> ((pmode k 2, ) <$> readMemory (i + 2))

parseOp3 ::
  Members [State Program, Error IntError] r =>
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
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r Int
readParam (ImmMode, val) = return val
readParam (PosMode, addr) = readMemory addr
readParam (RelMode, reladdr) = do
  base <- gets relativeBase
  readMemory (base + reladdr)

address ::
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r MAddress
address (PosMode, addr) = return addr
address (RelMode, reladdr) = (+ reladdr) <$> gets relativeBase
address (_, _) = throw (IntError ProgramError "Expected positional parameter")

opCombine2Write1 ::
  Members [State Program, Error IntError] r =>
  (Int -> Int -> Int) ->
  Parameter ->
  Parameter ->
  Parameter ->
  Sem r ()
opCombine2Write1 f p1 p2 p3 = do
  addr <- address p3
  k <- f <$> readParam p1 <*> readParam p2
  writeMemory addr k
  moveIpBy 4

haltProgram :: Member (State Program) r => Sem r ()
haltProgram = modify (\s -> s {running = False})

opReadInput ::
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r ()
opReadInput p = do
  addr <- address p
  inp <- gets input
  (x, input') <- maybe (throw (IntError InputIOError "")) return $
    (,) <$> headMaybe inp <*> tailMaybe inp
  writeMemory addr x
  modify (\s -> s {input = input'})
  moveIpBy 2

opWriteOutput ::
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r ()
opWriteOutput p = do
  v <- readParam p
  modify (\s -> s {output = v:output s})
  moveIpBy 2

opJumpCondition1 ::
  Members [State Program, Error IntError] r =>
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

opModifyRelBase ::
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r ()
opModifyRelBase p = do
  d <- readParam p
  modify (\s -> s{relativeBase = (relativeBase s + d)})
  moveIpBy 2

runStep ::
  Members [State Program, Error IntError] r =>
  Sem r ()
runStep = do
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
    9 -> parseOp1 k opModifyRelBase
    99 -> haltProgram
    x -> throw (IntError ProgramError ("Unknown opcode: " <> showt x))

initProgram :: Memory -> [Int] -> Program
initProgram intCode input =
  Program
    { ip = 0,
      memory = intCode,
      running = True,
      relativeBase = 0,
      input = input,
      output = [],
      err = Nothing
    }

runUntilEnd ::
  Members [State Program, Error IntError] r =>
  Sem r () ->
  Sem r ()
runUntilEnd step = do
  r <- gets running
  when r $ do
    step
    runUntilEnd step

runDebug ::
  Members [State Program, Error IntError, Embed IO] r =>
  Sem r ()
runDebug = do
  prg <- get @Program
  embed (print prg)
  embed getLine
  r <- gets running
  when r $ do
    runStep
    runDebug

haltWithError ::
  Members '[State Program] r =>
  IntError ->
  Sem r ()
haltWithError msg = do
  modify (\s -> s {err = Just msg} )
  haltProgram

runProgram :: Program -> Program
runProgram program = fst $
  runUntilEnd runStep
    & runError
    & fmap (either haltWithError return)
    & join
    & runState program
    & run

runProgramStep ::
  Program ->
  Program
runProgramStep prog = fst $
  runStep
    & runError
    & fmap (either haltWithError return)
    & join
    & runState prog
    & run

ignoreIO ::
  Members [State Program, Error IntError] r =>
  Sem r () ->
  Sem r ()
ignoreIO action =
  catch
    action
    ( \case
        IntError InputIOError _ -> return ()
        x -> throw x
    )
  
runProgramStepIgnoreIO ::
  Program ->
  Program
runProgramStepIgnoreIO prog = fst $
  f
    & runError
    & fmap (either haltWithError return)
    & join
    & runState prog
    & run
  where f = ignoreIO runStep 

runProgramDebug :: Program -> IO ()
runProgramDebug program =
  runDebug
    & runError
    & fmap (either haltWithError return)
    & join
    & runState program
    & runM
    & (print =<<) . fmap fst

runIntCode :: Memory -> [Int] -> (Program, Either IntError [Int])
runIntCode intCode input =
  let prg = runProgram (initProgram intCode input)
   in case err prg of
        Nothing -> (prg, Right (reverse (output prg)))
        Just msg -> (prg, Left msg)

parseProblem :: Text -> [Int]
parseProblem = T.split (== ',') >>> fmap (read . T.unpack)

initMemory :: [Int] -> Memory
initMemory xs = Map.fromList $ zip [0..] xs

solvePart1 :: Memory -> Maybe Int
solvePart1 memory = headMaybe . output . fst $ runIntCode memory [1]

solvePart2 :: Memory -> Maybe Int
solvePart2 memory = headMaybe . output . fst $ runIntCode memory [2]

main = do
  t <- readFileUtf8 "inputs/day9.txt"
  let memory = initMemory (parseProblem t)
  print $ solvePart1 memory
  print $ solvePart2 memory
