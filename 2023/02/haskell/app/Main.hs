module Main where

import Data.Char (isDigit)
import Data.List.Utils (contains, replace, split)

-- Types

data Game = Game
  { gameid :: Int,
    pulls :: [Pull]
  }

data Pull = Pull
  { red :: Int,
    green :: Int,
    blue :: Int
  }

-- Parsing

parseColour :: String -> [String] -> Int
parseColour = ((read . filter isDigit . headOrZero) .) . filter . contains
  where
    headOrZero [] = "0"
    headOrZero (x : _) = x

parsePull :: String -> Pull
parsePull x =
  let r = parseColour "red" c
      g = parseColour "green" c
      b = parseColour "blue" c
   in Pull r g b
  where
    c = split ", " x

splitLine :: [String] -> Game
splitLine [] = error "Empty line"
splitLine (x : xs) = Game (read x) $ map parsePull $ split "; " $ head xs

parseLine :: String -> Game
parseLine = splitLine . split ": " . replace "Game " ""

parseInput :: String -> [Game]
parseInput = map parseLine . lines

filterPull :: Pull -> Bool
filterPull x = red x <= 12 && green x <= 13 && blue x <= 14

-- Part 1

filterGames :: Game -> Bool
filterGames x = all filterPull (pulls x)

part1 :: [Game] -> String
part1 = show . sum . map gameid . filter filterGames

-- Part 2

getMax :: Game -> [Int]
getMax x = [maximum $ map red (pulls x), maximum $ map green (pulls x), maximum $ map blue (pulls x)]

part2 :: [Game] -> String
part2 = show . sum . map (product . getMax)

-- Main

main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  let input' = parseInput input
  putStrLn $ "Part 1: " ++ part1 input' ++ "\nPart 2: " ++ part2 input'
