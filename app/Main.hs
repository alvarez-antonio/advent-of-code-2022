module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import System.Environment (getArgs)
import Text.Printf

solution :: Int -> Int -> [String] -> Int
solution 1 1 = Day01.part1
solution 1 2 = Day01.part2
solution 2 1 = Day02.part1
solution 2 2 = Day02.part2
solution 3 1 = Day03.part1
solution 3 2 = Day03.part2
solution 4 1 = Day04.part1
solution 4 2 = Day04.part2
solution 5 1 = Day05.part1
solution 5 2 = Day05.part2

main :: IO ()
main = do
  args <- getArgs
  let day = (read :: String -> Int) (head args)
      part = (read :: String -> Int) (last args)
  fileStr <- readFile (printf "inputs/Day%02d-input" day)
  let fileLines = lines fileStr

  print (solution day part fileLines)
