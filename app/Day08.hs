module Day08 where

import Data.Char (digitToInt)
import Data.List (transpose, zip4, zipWith4)
import Data.Matrix (Matrix, fromLists, getCol, getElem, getRow, prettyMatrix)
import Data.Vector (toList)
import Debug.Trace (trace)

treeLineVisibility :: [Bool] -> Int -> [Int] -> [Bool]
treeLineVisibility visibility _ [] = visibility
treeLineVisibility visibility currentHeight treeLine
  | head treeLine > currentHeight = treeLineVisibility (visibility ++ [head treeLine > currentHeight]) (head treeLine) (tail treeLine)
  | otherwise = treeLineVisibility (visibility ++ [head treeLine > currentHeight]) currentHeight (tail treeLine)

part1 fileLines = do
  let width = length $ head fileLines
      depth = length fileLines
      forest = map (map digitToInt) fileLines
      leftVisibility = map (treeLineVisibility [] (-1)) forest
      rightVisibility = map (reverse . treeLineVisibility [] (-1) . reverse) forest
      topVisibility = transpose $ map (treeLineVisibility [] (-1)) (transpose forest)
      bottomVisibility = transpose $ map (reverse . treeLineVisibility [] (-1) . reverse) (transpose forest)
      visibility = map (\i -> zipWith4 (\l r t b -> l || r || t || b) (leftVisibility !! i) (rightVisibility !! i) (topVisibility !! i) (bottomVisibility !! i)) [0 .. (depth - 1)]
      visibleTrees = sum (map (sum . map fromEnum) visibility)
  visibleTrees

visibilityScore :: Int -> [Int] -> Int -> Int
visibilityScore startingHeight [] score = score
visibilityScore startingHeight treeLine score
  | startingHeight > head treeLine = visibilityScore startingHeight (tail treeLine) (score + 1)
  | otherwise = score + 1

visibilityScoreAtPoint :: Int -> Int -> Matrix Int -> Int
visibilityScoreAtPoint c r forest = do
  let height = getElem r c forest
      leftTreeLine = trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ "Starting Height: " ++ show height) reverse $ take (c - 1) $ toList $ getRow r forest
      leftScore = trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ "LeftTreeLine " ++ show leftTreeLine) visibilityScore height leftTreeLine 0
      rightTreeLine = drop c $ toList $ getRow r forest
      rightScore = trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ "RightTreeLine " ++ show rightTreeLine) visibilityScore height rightTreeLine 0
      topTreeLine = reverse $ take (r - 1) $ toList $ getCol c forest
      topScore = trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ "TopTreeLine " ++ show topTreeLine) visibilityScore height topTreeLine 0
      bottomTreeLine = drop r $ toList $ getCol c forest
      bottomScore = trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ "BottomTreeLine " ++ show bottomTreeLine) visibilityScore height bottomTreeLine 0
  trace ("Position: (" ++ show c ++ "," ++ show r ++ ") " ++ show [leftScore, topScore, rightScore, bottomScore] ++ " = " ++ show (leftScore * rightScore * topScore * bottomScore)) leftScore * rightScore * topScore * bottomScore

part2 :: [String] -> Int
part2 fileLines = do
  let width = length $ head fileLines
      depth = length fileLines
      forest = fromLists $ map (map digitToInt) fileLines
      visibilityScores = transpose $ map (\j -> map (\i -> visibilityScoreAtPoint j i forest) [1 .. width]) [1 .. depth]
      max = maximum $ map maximum visibilityScores
  trace ("Scores:\n" ++ prettyMatrix (fromLists visibilityScores) ++ "\n" ++ prettyMatrix forest) max