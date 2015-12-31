{-# LANGUAGE OverloadedStrings #-}

module Day7 where

import Debug.Trace (trace)

import Control.Applicative ((<|>))
import Control.Monad.Reader
import Data.Attoparsec.Text
import Data.Attoparsec.Combinator (many1)
import Data.Bits
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack, unpack)
import Data.Word (Word16)
import Data.Function.Memoize (memoize)

type Signal = Word16
type Wire = String

data Atom = Val Signal | Var Wire
  deriving Show

data Gate = LShift Atom Int
          | RShift Atom Int
          | Not Atom
          | And Atom Atom
          | Or Atom Atom
          | Basic Atom
  deriving Show

type Circuit = Map.Map Wire Gate

eval :: Circuit -> Wire -> Signal
eval circuit = me
  where e :: Wire -> Signal
        e target = case circuit Map.! target of
          (LShift atom amt) -> getAtom atom `shiftL` amt
          (RShift atom amt) -> getAtom atom `shiftR` amt
          (Not atom)        -> complement $ getAtom atom
          (And atom1 atom2) -> getAtom atom1 .&. getAtom atom2
          (Or  atom1 atom2) -> getAtom atom1 .|. getAtom atom2
          (Basic atom)      -> getAtom atom
        me = memoize e
        getAtom (Val i) = i
        getAtom (Var s) = me s

data Instruction = Instr {
  gate :: Gate,
  target :: Wire
}

parseWire :: Parser Wire
parseWire = unpack <$> takeTill isSpace

parseAtom :: Parser Atom
parseAtom = (Val <$> decimal) <|> (Var <$> parseWire)

parseBinOp :: Parser Gate
parseBinOp = do
  wire1 <- parseAtom
  op <- string " AND " <|> string " OR "
  wire2 <- parseAtom
  case op of
    " AND " -> return $ And wire1 wire2
    " OR "  -> return $ Or  wire1 wire2

parseNot :: Parser Gate
parseNot = string "NOT " >> Not <$> parseAtom

parseShift :: Parser Gate
parseShift = do
  atom <- parseAtom
  space
  direction <- char 'R' <|> char 'L'
  string "SHIFT "
  amt <- decimal
  case direction of
    'R' -> return $ RShift atom amt
    'L' -> return $ LShift atom amt

parseGate :: Parser Gate
parseGate = parseBinOp
        <|> parseNot
        <|> parseShift
        <|> Basic <$> parseAtom

instructionParser :: Parser (Wire, Gate)
instructionParser = do
  gate <- parseGate
  string " -> "
  wire <- takeText
  return (unpack wire, gate)

parseCircuit :: [String] -> Circuit
parseCircuit input = case mapM (parseOnly instructionParser . pack) input of
  Right r -> Map.fromList r
  Left err-> error err

solution :: String -> IO ()
solution input = do
  let circuit = parseCircuit (lines input)
  let a = eval circuit "a"
  print a
  let newcircuit = Map.insert "b" (Basic (Val a)) circuit
  print $ eval newcircuit "a"
