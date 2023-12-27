module Main where

import Control.Monad (ap)
import Data.IntSet qualified as S
import Text.Parsec qualified as P

type Parser = P.Parsec String ()

parse :: Parser a -> String -> a
parse p = either (error . show) id . P.parse p ""

data Card = Card
  { _id :: Int,
    _winning :: [Int],
    _own :: [Int]
  }
  deriving (Show)

-- Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
cardParser :: Parser Card
cardParser = do
  P.string "Card" >> P.spaces
  cardId <- read <$> P.many1 P.digit
  P.char ':' >> P.spaces
  winning <- P.many1 $ read <$> P.many1 P.digit <* P.spaces
  P.spaces >> P.char '|' >> P.spaces
  own <- P.many1 $ read <$> P.many1 P.digit <* P.spaces
  pure $ Card {_id = cardId, _winning = winning, _own = own}

parseLines :: String -> [Card]
parseLines = parse (P.many1 cardParser)

points :: Int -> Int
points x = if x == 0 then 0 else 2 ^ (x - 1)

wins :: Card -> Int
wins = points . length . S.toList . ap (S.intersection . S.fromList . _winning) (S.fromList . _own)

part1 :: String -> String
part1 = show . sum . map wins . parseLines

part2 :: String -> String
part2 = const "Not implemented"

main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  putStrLn $ "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input
