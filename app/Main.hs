module Main where

import Control.Arrow.BrainFuck

helloWorld :: Program
helloWorld = parse "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."

someFunc :: IO ()
someFunc = runBFArrow (runProgram $ helloWorld) State {pre = repeat 0, stack = repeat 0} 0 >>= \(pos, state) -> putStrLn (show pos)

main :: IO ()
main = someFunc
