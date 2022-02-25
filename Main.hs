module Main where

import Dictionary
import Wordle
import Text.Printf

main :: IO ()
main = do
  putStrLn "Play Wordl!"
  round <- next newGame
  gameplay round

next :: Round -> IO Round
next go = do
  putStrLn "Enter guess:"
  input <- getLine
  result <- withInput go input
  return result

gameplay :: Round -> IO ()
gameplay go = do
  let round = evaluate go
  if (guess go == "q") then do
    putStrLn (printf "Quitter! The answer was '%s'" (solution go))
  else if (solved round == True) then do
    putStrLn (printf "Congrats! Got it in %d" (iteration go))
  else do
    putStrLn (printScore round)
    go <- next round
    gameplay go

