module Lib
    ( someFunc
    ) where

import           Prelude          hiding (id, (.))

import           Data.Array
import           Data.Char
import           Data.Int
import           Data.List
import           Data.Tuple
import           Data.Either

import           Control.Arrow
import           Control.Category

import           Debug.Trace

data Op = IncPtr | DecPtr |
          IncVal | DecVal |
          PutCh | GetCh |
          LoopStart (Maybe Int64) | LoopEnd (Maybe Int64)
          deriving (Show)

preParseOps :: String -> [Op]
preParseOps = Prelude.map preParseSingle
  where
    preParseSingle :: Char -> Op
    preParseSingle '>' = IncPtr
    preParseSingle '<' = DecPtr
    preParseSingle '+' = IncVal
    preParseSingle '-' = DecVal
    preParseSingle '.' = PutCh
    preParseSingle ',' = GetCh
    preParseSingle '[' = LoopStart Nothing
    preParseSingle ']' = LoopEnd Nothing

type BraceIndexes = (Start, End)
type IndexedOp = (Int64, Op)
type Start = Int64
type End = Int64

type Program = Array Int64 Op

calculateBraceIndexes :: [Op] -> [BraceIndexes]
calculateBraceIndexes ops = let indexedOps = zip [0..] ops
                            in snd $ foldl' (flip handleOpenClose) ([],[]) indexedOps
                            where
                              handleOpen :: IndexedOp -> [Start] -> [Start]
                              handleOpen (idx,LoopStart _) otherStarts = idx:otherStarts
                              handleOpen _ otherStarts                 = otherStarts

                              handleClose :: IndexedOp -> ([Start],[BraceIndexes]) -> ([Start],[BraceIndexes])
                              handleClose (idx,LoopEnd _) (curStart:restStarts, otherBraceIndexes) = (restStarts, (curStart, idx):otherBraceIndexes)
                              handleClose _ otherResults = otherResults

                              handleOpenClose :: IndexedOp -> ([Start],[BraceIndexes]) -> ([Start],[BraceIndexes])
                              handleOpenClose indexedOp = first (handleOpen indexedOp) >>> handleClose indexedOp

parse :: String -> Program
parse codeStr =
  let ops = preParseOps codeStr
      braces = calculateBraceIndexes ops
      newStarts = map (\(start, end) -> (start, LoopStart (Just end))) braces
      newEnds = map (\(start, end) -> (end, LoopEnd (Just start))) braces
      progPrototype = listArray (0, fromIntegral (length ops) - 1) ops
  in (progPrototype//newStarts)//newEnds

data State = State {
  pre   :: [Int64],
  stack :: [Int64]
} deriving (Show)

curVal :: State -> Int64
curVal state = let stck = stack state
               in if null stck then 0 else head stck

assoc :: ((a, b), c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))

unassoc :: (a,(b,c)) -> ((a, b), c)
unassoc (a,(b,c)) = ((a,b),c)

data BFArrow a b = BFArrow { kleisli :: Kleisli IO (a, State) (b, State) }

instance Category BFArrow where
  id = BFArrow { kleisli = id }
  f . g = BFArrow { kleisli = kleisli f . kleisli g }

instance Arrow BFArrow where
  arr f = BFArrow { kleisli = first (arr f) }
  first f = BFArrow { kleisli = arr swapsnd >>> first (kleisli f) >>> arr swapsnd }
    where
      swapsnd :: ((a, b), c) -> ((a, c), b)
      swapsnd ~(~(x, y), z) = ((x, z), y)

instance ArrowLoop BFArrow where
  loop f = BFArrow {kleisli = loop (arr assoc >>> second (arr swap) >>> arr unassoc
                                    >>> kleisli f
                                    >>> arr assoc >>> second (arr swap) >>> arr unassoc)
                   }

instance ArrowApply BFArrow where
  app = first (arr kleisli) >>> BFArrow {
          kleisli = arr assoc >>> app
        }

fromLeft (Left a) = a
fromRight (Right b) = b

-- idea from https://hackage.haskell.org/package/arrows-0.4.4.1/docs/src/Control-Arrow-Transformer-State.html#StateArrow
instance ArrowChoice BFArrow where
  left f = BFArrow {
            kleisli = arr distr >>> left (kleisli f) >>> arr undistr
          } where
            distr (Left y, s) = Left (y, s)
            distr (Right z, s) = Right (z, s)
            undistr (Left (y, s)) = (Left y, s)
            undistr (Right (z, s)) = (Right z, s)

mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f = arr listcase >>>
         arr (const []) ||| (f *** mapArr f >>> arr (uncurry (:)))
         where listcase []     = Left ()
               listcase (x:xs) = Right (x,xs)

liftState :: Kleisli IO State State -> BFArrow a a
liftState kleisli = BFArrow { kleisli = second kleisli }

stateArr :: (State -> State) -> BFArrow a a
stateArr fn = liftState (arr fn)

liftFromKleisli :: Kleisli IO a b -> BFArrow a b
liftFromKleisli arrow = BFArrow { kleisli = first arrow }

unliftState :: BFArrow a b -> Kleisli IO (a, State) (b, State)
unliftState = kleisli

runBFArrow :: BFArrow a b -> State -> a -> IO (b, State)
runBFArrow arrow state a = runKleisli (unliftState arrow) (a, state)

toChar :: Int64 -> Char
toChar val = toEnum (fromIntegral val)

interpret :: Op -> BFArrow Int64 Int64
interpret IncPtr = stateArr (\state -> let (cur:rest) = stack state in State {pre = cur:pre state, stack = rest}) >>> arr (+1)
interpret DecPtr = stateArr (\state -> let (cur:rest) = pre state in State {stack = cur:stack state, pre = rest}) >>> arr (+1)
interpret IncVal = stateArr (\state -> let (cur:rest) = stack state in State {pre = pre state, stack = (cur + 1):rest}) >>> arr (+1)
interpret DecVal = stateArr (\state -> let (cur:rest) = stack state in State {pre = pre state, stack = (cur - 1):rest}) >>> arr (+1)
interpret PutCh = liftState (Kleisli (\state -> let (cur:rest) = stack state
                                                  in putStr (show (toChar cur)) >>
                                                  return State {
                                                    pre = pre state,
                                                    stack = cur:rest
                                                  }
                                        )) >>> arr (+1)
interpret GetCh = liftState (Kleisli (\state -> getChar >>=
                                                     \val -> return State {
                                                        pre = pre state,
                                                        stack = fromIntegral (digitToInt val):drop 1 (stack state)
                                                    }
                                        )) >>> arr (+1)
interpret (LoopStart (Just endPos)) = BFArrow { kleisli = second (arr (\state ->
                                                                      if curVal state == 0
                                                                      then (arr (const endPos), state)
                                                                      else (arr (+1), state)))
                                                           >>> arr unassoc
                                                           >>> first (arr swap >>> app)
                                             }
interpret (LoopEnd (Just startPos)) = BFArrow { kleisli = second (arr (\state ->
                                                                     if curVal state /= 0
                                                                     then (arr (const startPos), state)
                                                                     else (arr (+1), state)))
                                                          >>> arr unassoc
                                                          >>> first (arr swap >>> app)
                                            }
--interpret x = error $ "unexpected " ++ (show x)

inBounds :: Array Int64 e -> Int64 -> Bool
inBounds array idx = let (lo, hi) = bounds array in idx >= lo && idx <= hi

runProgram :: Program -> BFArrow Int64 Int64
runProgram program =
    arr inBoundsCase >>>
    arr id ||| (arr (\idx -> (interpret (program!idx), idx)) >>> app >>> runProgram program)
        where
          inBoundsCase idx
            | program `inBounds` idx = Right idx
            | otherwise = Left idx

helloWorld :: Program
helloWorld = parse "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

someFunc :: IO ()
someFunc = runBFArrow (runProgram $ helloWorld) State {pre = repeat 0, stack = repeat 0} 0 >>= \(pos, state) -> putStrLn (show pos)
