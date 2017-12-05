module Control.Arrow.BrainFuck.BFArrow(
  module Control.Arrow,

  State(..),
  curVal,

  BFArrow(..),
  assoc,
  unassoc,
  liftState,
  unliftState,
  stateArr,
  liftFromKleisli,
  runBFArrow
) where

import           Control.Arrow
import           Control.Category
import           Data.Int
import           Data.Tuple       (swap)

import           Prelude          hiding (id, (.))

data State = State {
  pre   :: [Int64],
  stack :: [Int64]
} deriving (Show)

data BFArrow a b = BFArrow { kleisli :: Kleisli IO (a, State) (b, State) }

curVal :: State -> Int64
curVal state = let stck = stack state
               in if null stck then 0 else head stck

assoc :: ((a, b), c) -> (a,(b,c))
assoc ((a,b),c) = (a,(b,c))

unassoc :: (a,(b,c)) -> ((a, b), c)
unassoc (a,(b,c)) = ((a,b),c)

{-
  Type Instances for BFArrow
-}

instance Category BFArrow where
  id = BFArrow { kleisli = id }
  f . g = BFArrow { kleisli = kleisli f . kleisli g }

-- idea from https://hackage.haskell.org/package/arrows-0.4.4.1/docs/src/Control-Arrow-Transformer-State.html#StateArrow
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

-- idea from https://hackage.haskell.org/package/arrows-0.4.4.1/docs/src/Control-Arrow-Transformer-State.html#StateArrow
instance ArrowChoice BFArrow where
  left f = BFArrow {
            kleisli = arr distr >>> left (kleisli f) >>> arr undistr
          } where
            distr (Left y, s)  = Left (y, s)
            distr (Right z, s) = Right (z, s)
            undistr (Left (y, s))  = (Left y, s)
            undistr (Right (z, s)) = (Right z, s)

{-
  Functions to use BFArrow with
-}

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
