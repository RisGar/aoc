module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

parseDigits :: String -> [Int]
parseDigits = map digitToInt . filter isDigit

parseDigits' :: String -> [Int]
parseDigits' = mapMaybe parseDigit' . tails

parseDigit' :: String -> Maybe Int
parseDigit' [] = Nothing
parseDigit' xxs@(x : _)
  | f "one" = Just 1
  | f "two" = Just 2
  | f "three" = Just 3
  | f "four" = Just 4
  | f "five" = Just 5
  | f "six" = Just 6
  | f "seven" = Just 7
  | f "eight" = Just 8
  | f "nine" = Just 9
  | isDigit x = Just (digitToInt x)
  | otherwise = Nothing
  where
    f = flip isPrefixOf xxs

headPlusTail :: [Int] -> Int
headPlusTail [] = error "Empty list"
headPlusTail xxs@(x : _) = x * 10 + last xxs

part1 :: [String] -> String
part1 = show . sum . map (headPlusTail . parseDigits)

part2 :: [String] -> String
part2 = show . sum . map (headPlusTail . parseDigits')

main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  print $ "Part 1: " ++ (part1 . lines) input
  print $ "Part 2: " ++ (part2 . lines) input
