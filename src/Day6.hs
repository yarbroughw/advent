module Day6 where

import Control.Applicative ((<|>))
import Text.Parsec (parse, ParseError)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, satisfy, string)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Prim (try)
import Data.Set (size, difference, union, fromList, Set, empty)
import Control.Monad.State

type Coord = (Int, Int)
type Light = (Int, Int)

through :: Coord -> Coord -> Set Light
through (x,y) (x',y') = fromList [ (a,b) | a <- [x..x'], b <- [y..y']]

data Action = TurnOn
            | TurnOff
            | Toggle
  deriving Show

data Instruction = Instr Action Coord Coord
  deriving Show

parseAction :: Parser Action
parseAction = (try (string "turn on")  >> return TurnOn)
          <|> (try (string "turn off") >> return TurnOff)
          <|> (try (string "toggle")   >> return Toggle)

parseCoord :: Parser Coord
parseCoord = do
  x <- read <$> many1 digit
  char ','
  y <- read <$> many1 digit
  return (x,y)

readInstruction :: Parser Instruction
readInstruction = do
  action <- parseAction
  char ' '
  start <- parseCoord
  string " through "
  end <- parseCoord
  return (Instr action start end)

runInstruction :: Instruction -> State (Set Light) ()
runInstruction (Instr action (x,y) (x',y')) = case action of
  TurnOn  -> state $ \lights -> ((), lights `union` newlights)
  TurnOff -> state $ \lights -> ((), difference lights newlights)
  Toggle  -> state $ \lights -> ((), symmetric_diff lights newlights)
  where newlights = (x,y) `through` (x', y')
        symmetric_diff a b = difference a b `union` difference b a

endState :: [Instruction] -> Set Light
endState instructions = execState (mapM_ runInstruction instructions) empty

solution :: String -> IO ()
solution input =
  case mapM (parse readInstruction "") $ lines input of
    Left err -> print err
    Right instructions -> print $ size $ endState instructions
