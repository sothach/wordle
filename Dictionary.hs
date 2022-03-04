module Dictionary (contains,randomWord) where

import System.Random
import System.IO.Unsafe

wordList :: IO [String]
wordList = do
    contents <- readFile "words.txt"
    return (lines contents)

dictionary :: [String]
dictionary = unsafePerformIO wordList

contains :: String -> Bool
contains word = any(\entry -> entry == word) dictionary

chooseRandomWord :: IO String
chooseRandomWord = do
  fmap (dictionary !!) $ randomRIO (0, length dictionary - 1)

randomWord :: String
randomWord = unsafePerformIO chooseRandomWord