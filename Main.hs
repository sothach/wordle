module Main where

import Text.Printf
import System.IO
import Data.List (intercalate)
import Wordle
import Dictionary
import AnsiColor
import Helpers

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Play Wordl!"
  round <- next (newGame randomWord)
  gameplay round

next :: Round -> IO Round
next go = do
  putStr (white ++ show (iteration go) ++ blue ++ " Enter guess: "++white)
  input <- getLine
  if(input == "q" || Dictionary.contains input) then do
    withInput go input
  else do
    putStrLn (printf "%s'%s' not in dictionary" red input)
    next go

gameplay :: Round -> IO ()
gameplay go = do
  let round = evaluate go
  if (guess go == "q") then do
    putStrLn (printf "%sQuitter! The answer was '%s'" magenta (solution go))
  else if (solved round == True) then do
    putStrLn (printf "%sCongrats! Got it in %d" lgreen (iteration round - 1))
  else do
    putStrLn (feedback round)
    go <- next round
    gameplay go

feedback :: Round -> String
feedback round = do
    let oop = intercalate "|" (map(\s -> foldl (\acc c -> acc ++ [c]) " " s) (misplaced round))
    "  " ++ green ++ "[" ++ (pattern round) ++ "] " ++ yellow ++ "misplaced: [" ++ oop ++ "] "
         ++ magenta ++ "exclude: [" ++ (set2string (excludes round)) ++ "]"