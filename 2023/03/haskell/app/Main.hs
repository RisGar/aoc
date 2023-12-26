module Main where

parseInput :: String -> String
parseInput = id

main :: IO ()
main = do
  input <- readFile "input.txt"
  let input' = parseInput input
  print $ "Hello"
