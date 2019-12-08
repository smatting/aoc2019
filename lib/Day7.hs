{-# LANGUAGE ScopedTypeVariables, TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, TypeApplications #-}
module Day7
where

import Prelude hiding ((!!))

import RIO hiding (catch)
import RIO.List
import qualified RIO.Text as T
import Control.Arrow ((>>>))

import qualified RIO.Vector.Unboxed as VU
import RIO.Vector.Unboxed ((!?))
import RIO.Vector.Unboxed.Partial ((//))

import RIO.List.Partial (tail)

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
  deriving (Show, Eq, Ord)

type Parameter = (ParameterMode, Int)

assertIndex ::
  Member (Error IntError) r =>
  VU.Vector Int ->
  Int ->
  Sem r ()
assertIndex v i
  | 0 <= i && i < VU.length v = return ()
  | otherwise = throw (IntError MemoryAccessError ("Intcode: Memory access error @ " <> showt i))

writeMemory ::
  Members [State Program, Error IntError] r =>
  MAddress ->
  MValue ->
  Sem r ()
writeMemory addr val = do
  m <- gets memory
  assertIndex m addr
  modify (\s -> s {memory = m // [(addr, val)]})

assertJust ::
  Member (Error IntError) r =>
  Text ->
  Maybe a ->
  Sem r a
assertJust msg Nothing = throw (IntError GenericError msg)
assertJust _ (Just x) = return x

(!!) :: Member (Error IntError) r => VU.Vector Int -> Int -> Sem r Int
(!!) v i = assertJust ("Intcode: Memory access error @ " <> showt i) (v !? i)

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

withPosParam ::
  Members [State Program, Error IntError] r =>
  Parameter ->
  (MAddress -> Sem r ()) ->
  Sem r ()
withPosParam (PosMode, addr) f = f addr
withPosParam (ImmMode, _) _ = haltWithError (IntError ProgramError "Expected positional parameter")

opCombine2Write1 ::
  Members [State Program, Error IntError] r =>
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
  Members [State Program, Error IntError] r =>
  Parameter ->
  Sem r ()
opReadInput p =
  withPosParam p $ \addr -> do
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
    99 -> haltProgram
    x -> haltWithError (IntError ProgramError ("Unknown opcode: " <> showt x))

initProgram :: VU.Vector Int -> [Int] -> Program
initProgram intCode input =
  Program
    { ip = 0,
      memory = intCode,
      running = True,
      input = input,
      output = [],
      err = Nothing
    }

haltWithError ::
  Members '[State Program] r =>
  IntError ->
  Sem r ()
haltWithError msg = do
  modify (\s -> s {err = Just msg} )
  haltProgram

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

runIntCode :: VU.Vector Int -> [Int] -> (Program, Either IntError [Int])
runIntCode intCode input =
  let prg = runProgram (initProgram intCode input)
   in case err prg of
        Nothing -> (prg, Right (reverse (output prg)))
        Just msg -> (prg, Left msg)

runAmp ::  VU.Vector Int -> Int -> Int -> Either IntError Int
runAmp prog phase input =
  either Left f (snd (runIntCode prog [phase, input]))
    where
      f [x] = Right x
      f _ = Left (IntError GenericError "Wrong output")

runPhases :: VU.Vector Int -> [Int] -> Either IntError Int
runPhases prog phases =
  let pipes = fmap (runAmp prog) phases
   in foldr (>=>) return pipes 0

solvePart1 :: VU.Vector Int -> Either IntError (Maybe Int)
solvePart1 input =
  maximumMaybe <$> traverse (runPhases input) (permutations [0..4])


communicate :: (a -> a -> (a, a)) -> [a] -> [a]
communicate f xs =
  reverse $ foldl' (\(y:ys) x -> let (y', x') = f y x in x':y':ys) [head xs] (tail xs)

communicateWrapped :: (a -> a -> (a, a)) -> [a] -> [a]
communicateWrapped f xs =
  let (x' : xs') = communicate f xs
      (t, x'') = f (last xs') x'
   in x'' : init (xs') ++ [t]

pipeIO :: Program -> Program -> (Program, Program)
pipeIO p1 p2
  | null (output p1) = (p1, p2)
  | otherwise = 
    ( p1 {output = []},
      p2 {input = input p2 ++ reverse (output p1)}
    )

runAndPipe :: Program -> Program -> (Program, Program)
runAndPipe p1 p2 = 
  let p1' = runProgramStepIgnoreIO p1
  in pipeIO p1' p2

runChain :: [Program] -> [Program]
runChain = communicateWrapped runAndPipe

allStopped :: [Program] -> Bool
allStopped = all (not . running)

runChainUntil :: [Program] -> [Program]
runChainUntil progs
  | allStopped progs = progs
  | otherwise = runChainUntil (runChain progs)

initPart2 :: VU.Vector Int -> [Int] -> [Program]
initPart2 intcode phases = zipWith f [0 ..] phases
  where
    f i phase =
      initProgram
        intcode
        (if i == 0 then [phase, 0] else [phase])

solvePart2 :: VU.Vector Int -> Maybe Int
solvePart2 intcode =
  join $ maximumMaybe <$> traverse (getOutput . runChainUntil . initPart2 intcode) (permutations [5 .. 9])
  where
    getOutput :: [Program] -> Maybe Int
    getOutput progs = headMaybe progs >>= \p -> headMaybe (input p)
  
parseProblem :: Text -> [Int]
parseProblem = T.split (== ',') >>> fmap (read . T.unpack)

main = do
  t <- readFileUtf8 "inputs/day7.txt"
  let intcode = (VU.fromList (parseProblem t))
  print (solvePart1 intcode)
  print (solvePart2 intcode)
