module Control.Arrow.BrainFuck.Program where

import Data.Int

import Data.Array

data Op = IncPtr | DecPtr |
          IncVal | DecVal |
          PutCh | GetCh |
          LoopStart (Maybe Int64) | LoopEnd (Maybe Int64)
          deriving (Show)

type BraceIndexes = (Start, End)
type IndexedOp = (Int64, Op)
type Start = Int64
type End = Int64

type Program = Array Int64 Op
