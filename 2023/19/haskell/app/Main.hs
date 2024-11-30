module Main where

import Control.Applicative (liftA3)
import Data.Function ((&))
import Data.Map qualified as M
import Text.Parsec ((<|>))
import Text.Parsec qualified as P
import Prelude hiding (compare)

-- Parsec

type Parser = P.Parsec String ()

parse :: Parser a -> String -> a
parse p = either (error . show) id . P.parse p ""

--- Structs

data Part = Part
  { _x :: Int,
    _m :: Int,
    _a :: Int,
    _s :: Int
  }
  deriving (Show)

data Workflow = Workflow
  { name :: String,
    rules :: [Rule],
    endAction :: Action
  }
  deriving (Show)

data Rule = Rule
  { cond :: Condition,
    action :: Action
  }
  deriving (Show)

data Condition = Condition
  { cat :: Category,
    comp :: Comparator,
    value :: Int
  }
  deriving (Show)

data Comparator = Lt | Gt deriving (Show)

data Category = X | M | A | S deriving (Show)

data Action = Action String | Reject | Accept deriving (Show)

type Workflows = M.Map String Workflow

--- Parsers

parsePart :: Parser Part
parsePart = do
  (x, m, a, s) <- (,,,) <$> (P.char '{' *> P.string "x=" *> P.many1 P.digit) <*> (P.string ",m=" *> P.many1 P.digit) <*> (P.string ",a=" *> P.many1 P.digit) <*> (P.string ",s=" *> P.many1 P.digit <* P.char '}')
  return $ Part (read x) (read m) (read a) (read s)

parseWorkflow :: Parser Workflow
parseWorkflow = do
  (name, rules, endAction) <- liftA3 (,,) (P.many1 P.letter <* P.char '{') (P.sepEndBy (P.try parseRule) (P.char ',')) (parseAction <* P.char '}')
  return $ Workflow name rules endAction

parseRule :: Parser Rule
parseRule =
  liftA2 Rule parseCondition parseAction

parseCondition :: Parser Condition
parseCondition = do
  (cat, comp, value) <- liftA3 (,,) parseCategory (Lt <$ P.char '<' <|> Gt <$ P.char '>') $ read <$> P.many1 P.digit <* P.char ':'
  return $ Condition cat comp value

parseCategory :: Parser Category
parseCategory =
  X <$ P.char 'x' <|> M <$ P.char 'm' <|> A <$ P.char 'a' <|> S <$ P.char 's'

parseAction :: Parser Action
parseAction =
  Reject <$ P.char 'R' <|> Accept <$ P.char 'A' <|> Action <$> P.many1 P.letter

parseWorkflows :: Parser [Workflow]
parseWorkflows = P.sepEndBy parseWorkflow (P.char '\n')

parseParts :: Parser [Part]
parseParts = P.sepEndBy parsePart (P.char '\n')

parseFile :: Parser (Workflows, [Part])
parseFile = do
  ws <- parseWorkflows <* P.char '\n'
  let ws' = M.fromList $ map ((,) =<< name) ws
  ps <- parseParts <* P.eof
  return (ws', ps)

-- Part 1

category :: Category -> Part -> Int
category X p = _x p
category M p = _m p
category A p = _a p
category S p = _s p

compare :: Condition -> Part -> Bool
compare (Condition cat Gt val) p = category cat p > val
compare (Condition cat Lt val) p = category cat p < val

followPart :: Workflows -> Workflow -> Part -> Int
followPart ws w p = do
  followRule $ rules w
  where
    followRule :: [Rule] -> Int
    followRule (r : rs) = if compare (cond r) p then followAction (action r) else followRule rs
    followRule [] = followAction (endAction w)

    followAction :: Action -> Int
    followAction a =
      case a of
        Accept -> _x p + _m p + _a p + _s p
        Reject -> 0
        Action a' -> followPart ws (ws M.! a') p

part1 :: (Workflows, [Part]) -> Int
part1 (ws, ps) = sum $ map (followPart ws (ws M.! "in")) ps

-- Part 2

part2 :: Workflows -> Int
part2 ws = 1

-- Main

main :: IO ()
main = do
  input <- readFile "../input/input.txt"
  let input' = parse parseFile input
  print $ part1 input'
  print $ part2 $ fst input'
