module Main where

import Numeric (readHex)
import Text.Parsec ((<|>))
import Text.Parsec qualified as P

type Parser = P.Parsec String ()

parse :: Parser a -> String -> a
parse p = either (error . show) id . P.parse p ""

data Direction = UpD | DownD | LeftD | RightD deriving (Show)

data Instruction = Instruction {direction :: Direction, distance :: Int} deriving (Show)

data Pos = Pos {_x :: Int, _y :: Int}

type Color = String

cardParser :: Parser Instruction
cardParser = do
  direction <- UpD <$ P.char 'U' <|> DownD <$ P.char 'D' <|> LeftD <$ P.char 'L' <|> RightD <$ P.char 'R'
  P.spaces
  distance <- read <$> P.many1 P.digit
  P.spaces >> P.char '(' >> P.char '#' >> P.many1 P.hexDigit >> P.char ')' >> P.spaces
  pure $ Instruction direction distance

getDirection' :: Char -> Direction
getDirection' '3' = UpD
getDirection' '1' = DownD
getDirection' '2' = LeftD
getDirection' '0' = RightD
getDirection' _ = error "Invalid direction"

cardParser' :: Parser Instruction
cardParser' = do
  P.letter >> P.spaces >> P.many1 P.digit >> P.spaces <* P.char '(' <* P.char '#'
  hex <- P.many1 P.hexDigit
  P.char ')' >> P.spaces
  let direction = getDirection' (last hex)
  let distance = fst . head $ readHex (take 5 hex)
  pure $ Instruction direction distance

parseLines :: String -> [Instruction]
parseLines = parse (P.many1 cardParser)

parseLines' :: String -> [Instruction]
parseLines' = parse (P.many1 cardParser')

followInstruction :: Pos -> Instruction -> Pos
followInstruction (Pos x y) (Instruction UpD d) = Pos x (y - d)
followInstruction (Pos x y) (Instruction DownD d) = Pos x (y + d)
followInstruction (Pos x y) (Instruction LeftD d) = Pos (x - d) y
followInstruction (Pos x y) (Instruction RightD d) = Pos (x + d) y

area :: [Instruction] -> Int
area = (`div` 2) . snd . foldl shoelace (Pos 0 0, 0)

picks :: [Instruction] -> Int
picks xs =
  let area' = area xs
      perimeter' = perimeter $ directionVectors xs
   in area' + (perimeter' `div` 2) + 1

shoelace :: (Pos, Int) -> Instruction -> (Pos, Int)
shoelace (Pos _x _y, acc) instr =
  let (Pos _x' _y') = followInstruction (Pos _x _y) instr
   in (Pos _x' _y', acc + (_y + _y') * (_x - _x'))

directionVectors :: [Instruction] -> [Pos]
directionVectors = map (followInstruction (Pos 0 0))

perimeter :: [Pos] -> Int
perimeter = sum . map (\(Pos x y) -> abs x + abs y)

part1 :: String -> String
part1 = show . picks . parseLines

part2 :: String -> String
part2 = show . picks . parseLines'

main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  putStrLn $ "Part 1: " ++ part1 input ++ "\nPart 2: " ++ part2 input
