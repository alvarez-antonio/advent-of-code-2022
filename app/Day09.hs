module Day09 where

import Data.Bits (Bits (xor))
import Data.List (intercalate, nub)
import Data.Matrix (matrix, prettyMatrix, setElem, toList)
import Debug.Trace (trace)

toStepsList inputLine = do
  let direction = head inputLine
      numberOfSteps = read $ drop 2 inputLine
  replicate numberOfSteps direction

applyStepToHead :: (Int, Int) -> Char -> (Int, Int)
applyStepToHead posH 'L' = (fst posH - 1, snd posH)
applyStepToHead posH 'U' = (fst posH, snd posH + 1)
applyStepToHead posH 'R' = (fst posH + 1, snd posH)
applyStepToHead posH 'D' = (fst posH, snd posH - 1)

isAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isAdjacent posH posT = abs (fst posH - fst posT) <= 1 && abs (snd posH - snd posT) <= 1

distRound :: Float -> Int
distRound x
  | x > 0 = ceiling x
  | x <= 0 = floor x

calculateDistanceVector :: (Float, Float) -> (Float, Float) -> (Int, Int)
calculateDistanceVector posH posT = do
  let disX = (fst posH - fst posT) / 2
      disY = (snd posH - snd posT) / 2
      moveX = distRound disX
      moveY = distRound disY
      moveVector = trace ("Move vec: " ++ show disX ++ " " ++ show disY) (moveX, moveY)
  moveVector

moveTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTail posH posT = do
  let posHFloat = (fromIntegral $ fst posH, fromIntegral $ snd posH)
      posTFloat = (fromIntegral $ fst posT, fromIntegral $ snd posT)
      distance = calculateDistanceVector posHFloat posTFloat
  trace ("Distance vector: " ++ show distance) (fst posT + fst distance, snd posT + snd distance)

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTail posH posT
  | posH == posT = posT
  | isAdjacent posH posT = posT
  | otherwise = moveTail posH posT

doStep :: [(Int, Int)] -> Char -> [(Int, Int)] -> [(Int, Int)]
doStep [pH, pT] step result = do
  let posH = applyStepToHead pH step
      posT = updateTail posH pT
  trace ("Step: " ++ show step ++ " Moved:" ++ show [posH, posT]) result ++ [posH, posT]
doStep initialPos step [] = do
  let posH = applyStepToHead (head initialPos) step
      posT = updateTail posH (last initialPos)
  trace ("Step: " ++ show step ++ " Moved:" ++ show [posH, posT]) doStep (tail initialPos) step [posH, posT]
doStep initialPos step result = do
  let posH = applyStepToHead (head initialPos) step
      posT = updateTail posH (head $ tail initialPos)
  trace ("Step: " ++ show step ++ " Moved:" ++ show [posH, posT]) doStep (tail initialPos) step result ++ [posT]

doAllStep :: [(Int, Int)] -> Char -> [(Int, Int)] -> [(Int, Int)]
doAllStep [] step result = trace ("Final result: " ++ show result) result
doAllStep remainingRope step [] = do
  let pos = applyStepToHead (head remainingRope) step
  trace ("Step: " ++ show step ++ " Moved:" ++ show [pos] ++ " Remaning Rope: " ++ show (tail remainingRope)) doAllStep (tail remainingRope) step [pos]
doAllStep remainingRope step result = do
  let pos = trace ("Result: " ++ show result ++ " Remaning Rope: " ++ show (tail remainingRope)) updateTail (last result) (head remainingRope)
  trace ("Step: " ++ show step ++ " Moved:" ++ show [pos] ++ " Remaning Rope: " ++ show (tail remainingRope)) doAllStep (tail remainingRope) step (result ++ [pos])

executeSteps :: ([(Int, Int)] -> Char -> [(Int, Int)] -> [(Int, Int)]) -> Char -> [[(Int, Int)]] -> [[(Int, Int)]]
executeSteps doStep step path = do
  let newMove = doStep (last path) step []
      updatedPath = path ++ [newMove]
  updatedPath

part1 fileLines = do
  let steps = reverse $ concatMap toStepsList fileLines
      initialPos = [(0, 0), (0, 0)] -- [posH, posT]
      path = foldr (executeSteps doStep) [initialPos] steps
      uniqueTailPos = length $ nub $ map last path
      board = matrix 15 15 (const '.')
      test = setElem 'T' (head $ nub $ map last path) board
      prettyPath = map prettyMatrix $ map (\[posH, posT] -> trace ("Matrix pos: " ++ show posH ++ show posT) setElem 'H' ((fst posH + 6), (snd posH + 6)) $ setElem 'T' ((fst posT + 6), (snd posT + 6)) board) path
  --   matrices = map (\pos -> trace ("Matrix pos: " ++ show pos) setElem 'T' pos board) (nub $ map last path)

  trace (intercalate "\n" prettyPath) uniqueTailPos

--   uniqueTailPos

--   trace (show (nub $ map last path)) 1

part2 fileLines = do
  let steps = reverse $ concatMap toStepsList fileLines
      initialPos = replicate 10 (0, 0) -- [posH, posT1, posT2..]
      path = foldr (executeSteps doAllStep) [initialPos] steps
      uniqueTailPos = length $ nub $ map last path
      board = matrix 15 15 (const '.')
      --   test = setElem 'T' (head $ nub $ map last path) board
      prettyPath = map prettyMatrix $ map (\[posH, posT1, posT2, posT3, posT4, posT5, posT6, posT7, posT8, posT9] -> setElem 'H' ((fst posH + 6), (snd posH + 6)) $ setElem '1' ((fst posT1 + 6), (snd posT1 + 6)) $ setElem '2' ((fst posT2 + 6), (snd posT2 + 6)) $ setElem '3' ((fst posT3 + 6), (snd posT3 + 6)) $ setElem '4' ((fst posT4 + 6), (snd posT4 + 6)) $ setElem '5' ((fst posT5 + 6), (snd posT5 + 6)) $ setElem '6' ((fst posT6 + 6), (snd posT6 + 6)) $ setElem '7' ((fst posT7 + 6), (snd posT7 + 6)) $ setElem '8' ((fst posT8 + 6), (snd posT8 + 6)) $ setElem '9' ((fst posT9 + 6), (snd posT9 + 6)) board) path
  --   matrices = map (\pos -> trace ("Matrix pos: " ++ show pos) setElem 'T' pos board) (nub $ map last path)

  --   trace (intercalate "\n" prettyPath) uniqueTailPos

  trace (show $ nub $ map last path) uniqueTailPos

--   trace (show (nub $ map last path)) 1