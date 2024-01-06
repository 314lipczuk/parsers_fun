{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseBoolExpr, parseNum, parse_program, BoolExpr(..), NumExpr(..), RelationalExpr(..) 
    ) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
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


-- Boolean expressions  --
--------------------------

data BoolExpr = 
  ConstBool Bool
  | IdentB String
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Relational RelationalExpr
  deriving (Eq, Ord, Show)

operatorTableBool :: [[Operator Parser BoolExpr]]
operatorTableBool = [
  [Prefix (Not <$ symbol "not")],
  [InfixL (And <$ symbol "and")],
  [InfixL (Or <$ symbol "or")]
  ]

parseBoolExpr :: Parser BoolExpr
parseBoolExpr = makeExprParser bTerm operatorTableBool

bTerm :: Parser BoolExpr
bTerm = choice [parens parseBoolExpr, parseConstBool, parseIdentB, parseRelationalExpr <&> Relational]

parseConstBool :: Parser BoolExpr
parseConstBool = ConstBool <$> (True <$ symbol "true" <|> False <$ symbol "false")

parseIdentB :: Parser BoolExpr
parseIdentB = IdentB <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- Relational expressions -- 
----------------------------
data RelationalExpr = 
  GreaterEquals NumExpr NumExpr
  | Greater NumExpr NumExpr
  | LessEquals NumExpr NumExpr
  | Less NumExpr NumExpr
  | Equals NumExpr NumExpr
  | NotEquals NumExpr NumExpr
  deriving (Eq, Ord, Show)

parseRelationalExpr :: Parser RelationalExpr
parseRelationalExpr = do
  ne1 <- numericalExprParser
  op <- symbol ">=" <|>  symbol ">" <|>  symbol "<=" <|>  symbol "<" <|>  symbol "=" <|>  symbol "<>"
  ne2 <- numericalExprParser
  return $ case op of 
    ">=" -> GreaterEquals ne1 ne2
    ">" -> Greater ne1 ne2
    "<=" -> LessEquals ne1 ne2
    "<" -> Less ne1 ne2
    "=" -> Equals ne1 ne2
    "<>" -> NotEquals ne1 ne2
    _ -> error "This should never happen"

-- Numerical expressions --
---------------------------
parseNum :: Parser NumExpr
parseNum = ConstNum <$> lexeme L.decimal

data NumExpr =
  ConstNum Int
  | IdentN String
  | Negation NumExpr
  | UnaryPlus NumExpr
  | Sum NumExpr NumExpr
  | Subtr NumExpr NumExpr
  | Product NumExpr NumExpr
  | Division NumExpr NumExpr
  | Modulo NumExpr NumExpr
  deriving (Eq, Ord, Show )

parseConstNum :: Parser NumExpr
parseConstNum = ConstNum <$> lexeme L.decimal

parseIdent ::  Parser NumExpr
parseIdent =  IdentN <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser NumExpr
pTerm = choice [parens numericalExprParser, parseConstNum, parseIdent]

operatorTable :: [[Operator Parser NumExpr]]
operatorTable = [
  [Prefix (Negation <$ symbol "-"), Prefix (UnaryPlus <$ symbol "+")],
  [InfixL (Product <$ symbol "*"), InfixL (Division <$ symbol "/"), InfixL (Modulo <$ symbol "%")],
  [InfixL (Sum <$ symbol "+"), InfixL (Subtr <$ symbol "-")]
  ]

numericalExprParser :: Parser NumExpr
numericalExprParser = makeExprParser pTerm operatorTable

-- Statements --
----------------



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
