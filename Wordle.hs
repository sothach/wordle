module Wordle (Round, evaluate, solution, solved, iteration, withInput,
               guess, pattern, excludes, misplaced, printScore, newGame) where

import Data.Set (Set, union)
import qualified Data.Set as Set
import Dictionary (randomWord)
import System.IO.Unsafe
import Debug.Trace (trace)
import Text.Printf
import Data.List

traces :: Show a => String -> a -> a
traces tag arg = trace (tag ++ show arg) arg

exists :: (Foldable t, Eq a) => a -> t a -> Bool
exists c str = any(\d -> d == c) str

data Round = Round { guess :: String
                   , pattern :: String
                   , excludes :: Set Char
                   , misplaced :: [Set Char]
                   , tried :: [String]
                   , iteration :: Int
                   , solution :: String
                   , solved :: Bool } deriving (Show)
newGame = Round { guess="", pattern="_____",
                   excludes=Set.empty,
                   misplaced=[Set.empty,Set.empty,Set.empty,Set.empty,Set.empty,Set.empty],
                   tried=[], iteration=0, solution=randomWord, solved=False}

printScore :: Round -> String
printScore round = do
    let excl = foldl (\acc c -> acc ++ [c]) "" (excludes round)
    let oop = intercalate "|" (map(\s -> foldl (\acc c -> acc ++ [c]) " " s) (misplaced round))
    "[" ++ (pattern round) ++ "] exclude: [" ++ excl ++ "] oops: [" ++ oop ++  "] (#" ++ (solution round)
      ++ "] => " ++ (show (solved round)) ++ "]"

withInput :: Round -> String -> IO Round
withInput go input = do
    return go { guess=input, tried=input : tried go, iteration=(iteration go)+1}

score :: Round -> Round
score go = do
      let zipped = zip (solution go) (guess go)
      let matched = foldl (\acc (a,b) -> acc ++ [if a == b then a else '_']) "" zipped
      let unused = Set.fromList (filter(\c -> all(\d -> d /= c) (solution go)) (guess go))
      let oop = map (\(a,b) -> if (a /= b && exists b (solution go)) then (Set.singleton b) else Set.empty) zipped
      let merged = map(\set -> Set.filter(\c -> not(exists c matched)) set) (map(\(a,b) -> Set.union a b) (zip oop (misplaced go)))
      go { guess=guess go, pattern=matched, excludes=Set.union unused (excludes go), misplaced=merged}

evaluate :: Round -> Round
evaluate go = do
    if guess go == solution go then go { solved=True} else score go
