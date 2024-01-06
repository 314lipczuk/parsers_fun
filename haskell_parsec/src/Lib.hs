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

parseNum :: Parsec Void Text NumExpr
parseNum = ConstNum <$> lexeme L.decimal

--newtype Identifier = Identifier Text deriving (Show, Eq)
--parseIdentifier :: Parsec Void Text Identifier
--parseIdentifier = Identifier <$> lexeme (takeWhile1P Nothing isAlpha)

--data Assignment = Assignment Identifier NumExpr deriving (Show, Eq)

data NumExpr =
  ConstNum Int
  | Ident String
  | Negation NumExpr
  | UnaryPlus NumExpr
  | Sum NumExpr NumExpr
  | Subtr NumExpr NumExpr
  | Product NumExpr NumExpr
  | Division NumExpr NumExpr
  | Modulo NumExpr NumExpr
  deriving (Eq, Ord, Show )

parseConstNum :: Parsec Void Text NumExpr
parseConstNum = ConstNum <$> lexeme L.decimal

parseIdent :: Parsec Void Text NumExpr
parseIdent =  Ident <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser NumExpr
pTerm = choice [parens pExpr, parseConstNum, parseIdent]

operatorTable :: [[Operator Parser NumExpr]]
operatorTable = [
  [Prefix (Negation <$ symbol "-"), Prefix (UnaryPlus <$ symbol "+")],
  [InfixL (Product <$ symbol "*"), InfixL (Division <$ symbol "/"), InfixL (Modulo <$ symbol "%")],
  [InfixL (Sum <$ symbol "+"), InfixL (Subtr <$ symbol "-")]
  ]

pExpr :: Parser NumExpr
pExpr = makeExprParser pTerm operatorTable

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
