module Main where

import Data.List.Split (splitOn)
import Day01
import System.Environment (getArgs)
import Text.Printf

parseInput :: String -> [[Int]]
parseInput fileStr = do
  let splits = lines fileStr
  let elfSplits = splitOn [""] splits
  let convertedSplits = map (map read) elfSplits
  convertedSplits

solution :: Int -> Int -> [[Int]] -> Int
solution 1 1 = Day01.part1
solution 1 2 = Day01.part2

main :: IO ()
main = do
  args <- getArgs
  let day = read (head args)
      part = read (last args)
  fileStr <- readFile (printf "app/inputs/Day%02d-input" day)
  let input = parseInput fileStr
      result = solution day part input

  print result
