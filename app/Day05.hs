module Day05 (part1, part2) where

import Data.Char (digitToInt, isSpace)
import Data.Foldable (toList)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Sequence (Seq, fromList, index, update)
import Debug.Trace (trace)

isStack :: String -> Bool
isStack stack
  | head stack == ' ' = False
  | otherwise = True

cleanStacks :: [String] -> [String]
cleanStacks rawStacks = do
  let transposedStacks = transpose rawStacks
      cleanedStacks = filter isStack transposedStacks
  map (filter (not . isSpace)) cleanedStacks

parseStacks :: [String] -> [String]
parseStacks rawStacks = do
  let stacks = reverse rawStacks
      cleanedStacks = trace ("Cleaned Stacks: " ++ show (cleanStacks stacks)) cleanStacks stacks
  cleanedStacks

destruct :: String -> (Int, Int, Int)
destruct move = do
  let pieces = splitOn " " move
  (read (pieces !! 1), read (pieces !! 3), read (pieces !! 5))

doInstructions :: Seq String -> Int -> Int -> Int -> Seq String
doInstructions stacks 0 source destination = stacks
doInstructions stacks howMany source destination = do
  let updatedDestination = (stacks `index` (destination - 1)) ++ [last (stacks `index` (source - 1))]
      updatedSource = init (stacks `index` (source - 1))
      firstUpdate = update (destination - 1) updatedDestination stacks
      updatedStacks = update (source - 1) updatedSource firstUpdate
  trace ("Stack updated " ++ show updatedStacks) doInstructions updatedStacks (howMany - 1) source destination

applyInstruction :: Seq String -> (Int, Int, Int) -> Seq String
applyInstruction stack instruction = do
  let (howMany, source, destination) = instruction
  trace ("Move to be processed " ++ show instruction) doInstructions stack howMany source destination

process :: Seq String -> [(Int, Int, Int)] -> Seq String
process stacks [] = stacks
process stacks instructions = do
  let newStacks = applyInstruction stacks (head instructions)
  process newStacks (tail instructions)

part1 :: [String] -> Int
part1 fileLines = do
  let firstSplit = splitOn [""] fileLines
      stacks = fromList $ parseStacks $ head firstSplit
      moves = trace ("Stacks " ++ show stacks) map destruct $ last firstSplit
      result = trace ("Moves " ++ show moves) process stacks moves
      list = trace ("Result " ++ show result) toList result
      sentence = map last list
  trace ("sentence " ++ show sentence) 1

part2 fileLines = 1