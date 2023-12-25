module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (tails, isPrefixOf)
import Data.Maybe (mapMaybe)

parseDigits :: String -> [Int]
parseDigits xs = map digitToInt $ filter isDigit xs

parseDigits' :: [String] -> [Int]
parseDigits' xs = mapMaybe parseDigit' xs

parseDigit' :: String -> Maybe Int
parseDigit' [] = Nothing
parseDigit' xxs@(x:_)
    | isPrefixOf "one" xxs = Just 1
    | isPrefixOf "two" xxs = Just 2
    | isPrefixOf "three" xxs = Just 3
    | isPrefixOf "four" xxs = Just 4
    | isPrefixOf "five" xxs = Just 5
    | isPrefixOf "six" xxs = Just 6
    | isPrefixOf "seven" xxs = Just 7
    | isPrefixOf "eight" xxs = Just 8
    | isPrefixOf "nine" xxs = Just 9
    | isDigit x = Just (digitToInt x)
    | otherwise = Nothing

headPlusTail :: [Int] -> Int
headPlusTail [] = error "Empty list"
headPlusTail xxs@(x:_) = x * 10 + last xxs

part1 :: [String] -> String
part1 xs = show . sum $ headPlusTail <$> map parseDigits xs

part2 :: [String] -> String
part2 xs = show . sum $ headPlusTail <$> map parseDigits' (map tails xs)


main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  print ("Part 1: " ++ (part1 $ lines input))
  print ("Part 2: " ++ (part2 $ lines input))
