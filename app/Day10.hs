module Day10 where

import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Debug.Trace (trace)

operation :: String -> [Int -> Int]
operation input
  | input == "noop" = [id]
  | otherwise = [id, (+) ((read :: String -> Int) (drop 5 input))]

part1 :: [String] -> Int
part1 fileLines = do
  let operations = concatMap operation fileLines
  let register = scanl (\arg f -> f arg) 1 operations
  let interestingCycles = [20, 60, 100, 140, 180, 220]
  let values = map (\c -> register !! (c - 1) * c) interestingCycles
  trace ("Register " ++ show register ++ "\nValues :" ++ show (map (\c -> register !! (c - 1)) interestingCycles)) sum values

charToDraw :: Int -> Int -> Char
charToDraw cycle pos
  | abs (cycle - pos) > 1 = trace (show (cycle, pos) ++ ": . " ++ show (cycle - pos)) '.'
  | otherwise = trace (show (cycle, pos) ++ ": # " ++ show (cycle - pos)) '#'

part2 fileLines = do
  let operations = concatMap operation fileLines
  let register = scanl (\arg f -> f arg) 1 operations
  let toDraw = zip (map (`rem` 40) [0 .. 259]) register
  let pixels = trace (show toDraw) map (uncurry charToDraw) toDraw
  let lcd = intercalate "\n" $ chunksOf 40 lcd
  trace (show toDraw ++ "\n" ++ (intercalate "\n" $ chunksOf 40 $ pixels)) 1