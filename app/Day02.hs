module Day02 (part1, part2) where

import Data.List.Split (splitOn)

parseRockPaperScisors :: [String] -> ([String] -> Int) -> Int
parseRockPaperScisors fileLines scoreCalculator = do
  let turns = map (splitOn " ") fileLines
      scores = map scoreCalculator turns
      total = sum scores
  total

-- Win = 6
-- Draw = 3
-- Lose = 0
-- A => Rock = 1
-- B => Paper = 2
-- C => Scissors = 3
-- X => Rock
-- Y => Paper
-- Z => Scissors
part1Score :: [String] -> Int
part1Score ["A", "X"] = 4
part1Score ["A", "Y"] = 8
part1Score ["A", "Z"] = 3
part1Score ["B", "X"] = 1
part1Score ["B", "Y"] = 5
part1Score ["B", "Z"] = 9
part1Score ["C", "X"] = 7
part1Score ["C", "Y"] = 2
part1Score ["C", "Z"] = 6

-- Win = 6
-- Draw = 3
-- Lose = 0
-- A => Rock = 1
-- B => Paper = 2
-- C => Scissors = 3
-- X => Lose
-- Y => Draw
-- Z => Win
part2Score :: [String] -> Int
part2Score ["A", "X"] = 3
part2Score ["A", "Y"] = 4
part2Score ["A", "Z"] = 8
part2Score ["B", "X"] = 1
part2Score ["B", "Y"] = 5
part2Score ["B", "Z"] = 9
part2Score ["C", "X"] = 2
part2Score ["C", "Y"] = 6
part2Score ["C", "Z"] = 7

part1 :: [String] -> Int
part1 fileLines = parseRockPaperScisors fileLines part1Score

part2 :: [String] -> Int
part2 fileLines = parseRockPaperScisors fileLines part2Score