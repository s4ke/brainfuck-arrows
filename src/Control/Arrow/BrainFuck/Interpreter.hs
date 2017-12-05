module Control.Arrow.BrainFuck.Interpreter(
  runProgram
) where

import Control.Arrow.BrainFuck.BFArrow
import Control.Arrow.BrainFuck.Program

import Data.Array
import Data.Int
import           Data.Char
import Data.Tuple




runProgram :: Program -> BFArrow Int64 Int64
runProgram program =
    arr inBoundsCase >>>
    arr id ||| (arr (\idx -> (interpret (program!idx), idx)) >>> app >>> runProgram program)
        where
          inBoundsCase idx
            | program `inBounds` idx = Right idx
            | otherwise = Left idx



mapArr :: ArrowChoice arr => arr a b -> arr [a] [b]
mapArr f = arr listcase >>>
         arr (const []) ||| (f *** mapArr f >>> arr (uncurry (:)))
         where listcase []     = Left ()
               listcase (x:xs) = Right (x,xs)


toChar :: Int64 -> Char
toChar val = toEnum (fromIntegral val)

interpret :: Op -> BFArrow Int64 Int64
interpret IncPtr = stateArr (\state -> let (cur:rest) = stack state in State {pre = cur:pre state, stack = rest}) >>> arr (+1)
interpret DecPtr = stateArr (\state -> let (cur:rest) = pre state in State {stack = cur:stack state, pre = rest}) >>> arr (+1)
interpret IncVal = stateArr (\state -> let (cur:rest) = stack state in State {pre = pre state, stack = (cur + 1):rest}) >>> arr (+1)
interpret DecVal = stateArr (\state -> let (cur:rest) = stack state in State {pre = pre state, stack = (cur - 1):rest}) >>> arr (+1)
interpret PutCh = liftState (Kleisli (\state -> let (cur:rest) = stack state
                                                  in putChar (toChar cur) >>
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
