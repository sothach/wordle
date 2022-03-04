module Wordle where

import Data.Set (Set, union, singleton, empty, fromList)
import qualified Data.Set as Set
import Helpers
import Debug.Trace (trace)
import Text.Printf

data Round = Round { guess :: String
                   , pattern :: String
                   , excludes :: Set Char
                   , misplaced :: [Set Char]
                   , tried :: [String]
                   , iteration :: Int
                   , solution :: String
                   , solved :: Bool } deriving (Show)
newGame solution = Round { guess="", pattern="_____",
                   excludes=empty,
                   misplaced=[empty,empty,empty,empty,empty],
                   tried=[], iteration=1, solution=solution, solved=False}

withInput :: Round -> String -> IO Round
withInput go input = do
    return go { guess=input, tried=input : tried go, iteration=iteration go+1}

score :: Round -> Round
score go = do
      let zipped = zip (solution go) (guess go)
      let matched = foldl (\acc (a,b) -> acc ++ [if a == b then a else '_']) "" zipped
      let unused = fromList ([c | c <- guess go, not $ elem c (solution go)])
      let merge oop = [Set.filter(\c -> not $ elem c matched) set | set <- mergeSets $ zip oop (misplaced go)]
      let misplaced = map (\(a,b) -> if (a /= b && elem b (solution go)) then (singleton b) else empty) zipped
      go { guess=guess go, pattern=matched, excludes=union unused (excludes go), misplaced=merge misplaced}

evaluate :: Round -> Round
evaluate go = do
    if guess go == solution go then go { solved=True, pattern=(guess go)} else score go