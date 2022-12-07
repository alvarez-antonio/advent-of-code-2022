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

doInstructionOneAtATime :: Seq String -> Int -> Int -> Int -> Seq String
doInstructionOneAtATime stacks 0 source destination = stacks
doInstructionOneAtATime stacks howMany source destination = do
  let updatedDestination = (stacks `index` (destination - 1)) ++ [last (stacks `index` (source - 1))]
      updatedSource = init (stacks `index` (source - 1))
      firstUpdate = update (destination - 1) updatedDestination stacks
      updatedStacks = update (source - 1) updatedSource firstUpdate
  trace ("Stack updated " ++ show updatedStacks) doInstructionOneAtATime updatedStacks (howMany - 1) source destination

doInstructionInBulk :: Seq String -> Int -> Int -> Int -> Seq String
doInstructionInBulk stacks howMany source destination = do
  let sourceStack = stacks `index` (source - 1)
      destinationStack = stacks `index` (destination - 1)
      updatedDestination = destinationStack ++ drop (length sourceStack - howMany) sourceStack
      updatedSource = take (length sourceStack - howMany) sourceStack
      firstUpdate = update (destination - 1) updatedDestination stacks
      updatedStacks = update (source - 1) updatedSource firstUpdate
  trace ("Stack updated " ++ show updatedStacks) updatedStacks

applyInstruction :: (Seq String -> Int -> Int -> Int -> Seq String) -> Seq String -> (Int, Int, Int) -> Seq String
applyInstruction doInstructionFunction stack instruction = do
  let (howMany, source, destination) = instruction
  trace ("Move to be processed " ++ show instruction) doInstructionFunction stack howMany source destination

process :: (Seq String -> Int -> Int -> Int -> Seq String) -> Seq String -> [(Int, Int, Int)] -> Seq String
process doInstructionFunction stacks [] = stacks
process doInstructionFunction stacks instructions = do
  let newStacks = applyInstruction doInstructionFunction stacks (head instructions)
  process doInstructionFunction newStacks (tail instructions)

part1 :: [String] -> Int
part1 fileLines = do
  let firstSplit = splitOn [""] fileLines
      stacks = fromList $ parseStacks $ head firstSplit
      moves = trace ("Stacks " ++ show stacks) map destruct $ last firstSplit
      result = trace ("Moves " ++ show moves) process doInstructionOneAtATime stacks moves
      list = trace ("Result " ++ show result) toList result
      sentence = map last list
  trace ("sentence " ++ show sentence) 1

part2 :: [String] -> Int
part2 fileLines = do
  let firstSplit = splitOn [""] fileLines
      stacks = fromList $ parseStacks $ head firstSplit
      moves = trace ("Stacks " ++ show stacks) map destruct $ last firstSplit
      result = trace ("Moves " ++ show moves) process doInstructionInBulk stacks moves
      list = trace ("Result " ++ show result) toList result
      sentence = map last list
  trace ("sentence " ++ show sentence) 1