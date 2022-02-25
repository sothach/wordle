module Dictionary (randomWord) where

import System.Random
import System.IO.Unsafe

wordList :: IO [String]
wordList = do
    contents <- readFile "words.txt"
    return (lines contents)

chooseRandomWord :: IO String
chooseRandomWord = do
  words <- wordList
  fmap (words !!) $ randomRIO (0, length words - 1)

randomWord :: String
randomWord = unsafePerformIO chooseRandomWord