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
import Data.Functor (($>), (<&>))
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

data NumOp = Plus | Minus | Multiply | Divide | Modulo deriving (Show, Eq)
parseNumOp :: Parsec Void Text NumOp 
parseNumOp =
  (symbol "+" $> Plus)
  <|> (symbol "-" $> Minus)
  <|> (symbol "*" $> Multiply)
  <|> (symbol "/" $> Divide)
  <|> (symbol "%" $> Modulo)

parseNum :: Parsec Void Text NumExpr
parseNum = ConstNum <$> lexeme L.decimal

--parseJustNum :: Parsec Void Text Int
--parseJustNum = lexeme L.decimal

newtype Identifier = Identifier Text deriving (Show, Eq)
parseIdentifier :: Parsec Void Text Identifier
parseIdentifier = Identifier <$> lexeme (takeWhile1P Nothing isAlpha)

data Assignment = Assignment Identifier NumExpr deriving (Show, Eq)

-- num_expr = NUM | "-" num_expr | "+" num_expr | IDENT | num_expr num_op num_expr | "(" num_expr ")"
data PrefixOp = UnaryPlus | UnaryMinus deriving (Show, Eq)
parsePrefixOp :: Parsec Void Text PrefixOp
parsePrefixOp = (symbol "+" $> UnaryPlus) <|> (symbol "-" $> UnaryMinus)

data NumExpr =
  ConstNum Int
  | Ident Identifier
  | NumExpr NumExpr NumOp NumExpr
  | NumExprWithPrefix PrefixOp NumExpr
  | ParenthesisedNumExpr NumExpr
  deriving (Show, Eq)

parseNumExprWithPrefix :: Parsec Void Text NumExpr
parseNumExprWithPrefix = do
  op <- parsePrefixOp
  expr <- parseNumExpr
  return $ ParenthesisedNumExpr $ NumExprWithPrefix op expr

parseParenthesisedNumExpr :: Parsec Void Text NumExpr
parseParenthesisedNumExpr = do
  _ <- symbol "("
  expr <- parseNumExpr
  _ <- symbol ")"
  return $ ParenthesisedNumExpr expr

parseBinaryNumericalExpression :: Parsec Void Text NumExpr
parseBinaryNumericalExpression = do
  -- has to be like this, so that it's not left recursive
  e1 <- parseNumExprWithPrefix <|> parseNum <|> (Ident <$> parseIdentifier) <|> parseParenthesisedNumExpr
  op <- parseNumOp
  r2 <- parseNumExpr
  return $ NumExpr e1 op r2

parseNumExpr :: Parsec Void Text NumExpr
parseNumExpr =
  try parseBinaryNumericalExpression <|> parseParenthesisedNumExpr <|> try parseNumExprWithPrefix <|> try parseNum <|> (Ident <$> parseIdentifier)    

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
parseRelOp = undefined
