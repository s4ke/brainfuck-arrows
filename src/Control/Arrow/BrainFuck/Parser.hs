module Control.Arrow.BrainFuck.Parser(
  parse
) where

import           Control.Arrow.BrainFuck.Program

import           Control.Arrow

import           Data.Array
import           Data.List

parse :: String -> Program
parse codeStr =
  let ops = preParseOps codeStr
      braces = calculateBraceIndexes ops
      newStarts = map (\(start, end) -> (start, LoopStart (Just end))) braces
      newEnds = map (\(start, end) -> (end, LoopEnd (Just start))) braces
      progPrototype = listArray (0, fromIntegral (length ops) - 1) ops
  in (progPrototype//newStarts)//newEnds

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
