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

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

someFunc :: IO ()
someFunc = putStrLn "someFucc"

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

operatorTable :: [[Operator Parser Expr]]
operatorTable = [
    [ prefix "-" Negation, prefix "+" id ],
    [ binary "*" Product, binary "/" Division],
    [binary "+" Sum, binary "-" Subtr]
    ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)


parseEqualLen :: Parser String
parseEqualLen = do
  a <- char 'a'
  l <- optional parseEqualLen
  b <- char 'b'
  case l of 
    Nothing -> return ([a, b])
    Just l' -> return ([a] ++ l' ++ [b])

parseBrackets :: Parser String
parseBrackets = do
  char '('
  l <- optional $ many parseBrackets
  char ')'
  case l of 
    Nothing -> return ("()")
    Just l' -> return ("(" ++ show l'  ++ ")")
