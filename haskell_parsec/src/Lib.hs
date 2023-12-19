{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)
import Control.Monad.Combinators.Expr
import GHC.Conc (par)
import Data.Functor (($>))
import Data.Char (isAlpha)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

someFunc :: IO ()
someFunc = putStrLn "someFucc"

parseProgram = undefined


parseBoolExpr :: Parsec Void Text Bool
parseBoolExpr = undefined

data BoolOp = And | Or deriving (Show, Eq)
parseBoolOp :: Parsec Void Text BoolOp
parseBoolOp = (symbol "and" $> And) <|> (symbol "or" $> Or)


data RelOp = Plus | Minus | Multiply | Divide | Modulo deriving (Show, Eq)
parseRel :: Parsec Void Text RelOp
parseRel =
  (symbol "+" $> Plus)
  <|> (symbol "-" $> Minus)
  <|> (symbol "*" $> Multiply)
  <|> (symbol "/" $> Divide)
  <|> (symbol "%" $> Modulo)

parseNum :: Parsec Void Text Int
parseNum = do
  s <- optional ( symbol "+" <|> symbol "-")
  n <- lexeme L.decimal
  case s of
    Nothing -> return n
    Just "-" -> return (-n)
    Just "+" -> return n
    _ -> fail "Invalid number"

parseJustNum :: Parsec Void Text Int
parseJustNum = lexeme L.decimal

newtype Identifier = Identifier Text deriving (Show, Eq)
parseIdentifier :: Parsec Void Text Identifier
parseIdentifier = Identifier <$> lexeme (takeWhile1P Nothing isAlpha)

data Assignment = Assignment Identifier NumExpr deriving (Show, Eq)

-- num_expr = NUM | "-" num_expr | "+" num_expr | IDENT | num_expr num_op num_expr | "(" num_expr ")"

data NumExpr = 
  ConstNum Int
  | Ident Identifier
  | NumExpr NumExpr NumOp NumExpr
  | NegNumExpr NumExpr
  | ParenthesisedNumExpr NumExpr
  
parseNumExpr :: Parser Void Text NumExpr
parseNumExpr = 
  parseNum 
  <|> sequence [symbol "(", parseNumExpr, symbol ")"]

parse_simple_instr = undefined
parse_instr = undefined
parse_assign = undefined
parse_if_stat = undefined
parse_jmp = undefined
parse_output = undefined
parse_input = undefined
parseDeclaration = undefined
parseDeclarationList = undefined
parse_program = undefined
parseNumOp = undefined


