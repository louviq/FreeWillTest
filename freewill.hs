module FreeWill where

import Control.Monad
import System.IO
import Data.MultiSet (MultiSet, occur, insert, empty)

show01 :: Bool -> String
show01 False = "0"
show01 True = "1"

buildNgrams :: Int -> [a] -> [[a]]
buildNgrams n xs
  | n <= length xs = take n xs : buildNgrams n (tail xs)
  | otherwise = []

ngramDepth = 3

makePrediction :: [Bool] -> Bool
makePrediction pastInputs
  | length pastInputs < ngramDepth = True  -- Always guess 1 for this case for now
  | otherwise = prediction
  where
    prediction = if trues > falses then True else False
    trues = occur (prefix ++ [True]) multiset
    falses = occur (prefix ++ [False]) multiset
    prefix = reverse $ take (ngramDepth - 1) pastInputs
    ngrams = buildNgrams ngramDepth pastInputs
    multiset = foldl (flip insert) empty ngrams


computeAccuracy :: [Bool] -> [Bool] -> Float
computeAccuracy inputs predictions = (fromIntegral numCorrect) / (fromIntegral $ length inputs)
  where
    numCorrect = length $ filter id $ zipWith (==) inputs predictions



trial :: Int -> [Bool] -> [Bool] -> IO ([Bool], [Bool])
trial n pastInputs pastPredictions = do
  putStr $ "Running trial " ++ show n ++ ": "
  input <- getInput
  let inputs = input : pastInputs
  let predictions = prediction : pastPredictions
  putStr "\n"
  putStrLn $ "I predicted: " ++ show01 prediction
  putStrLn $ "You typed:   " ++ show01 input
  let accuracyPercent = (computeAccuracy inputs predictions) * 100.0
  putStrLn $ "My accuracy: " ++ show accuracyPercent ++ "%"
  return (inputs, predictions)
  where
    prediction = makePrediction pastInputs



getInput :: IO Bool
getInput = do
  inp <- getChar -- user prompt
  case inp of     -- parse user input, check its either 0 or 1
    '0'-> return False
    '1'-> return True
    _ -> error "Must type 0 or 1!"


run :: Int -> [Bool] -> [Bool] -> IO ()
run n pastInputs pastPredictions = do
  (inputs, predictions) <- trial n pastInputs pastPredictions
  run (n + 1) inputs predictions


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  run 0 [] []
