{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( 
   Decl(..)
  , parseLabelDecl
  ,parseProgramInstr
  ,parseProgramDecl
  , parseVarDecl
  , parseDeclaration
  , parseDeclarations
  , parseProgram,
      parseBlock,Ident(..) ,parseSerialInstr, parseInput, parseOutput,parseTag, parseIf, parseGoto, parseInstr, parseBoolExpr, parseNum,parseProgram, parseAssignment, Instr(..), BoolExpr(..), NumExpr(..), RelationalExpr(..) 
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
bTerm = choice [ parseConstBool, parseRelationalExpr <&> Relational, parseIdentB, parens parseBoolExpr]

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

parseIdentN ::  Parser NumExpr
parseIdentN =  IdentN <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser NumExpr
pTerm = choice [parens numericalExprParser, parseConstNum, parseIdentN]

operatorTable :: [[Operator Parser NumExpr]]
operatorTable = [
  [Prefix (Negation <$ symbol "-"), Prefix (UnaryPlus <$ symbol "+")],
  [InfixL (Product <$ symbol "*"), InfixL (Division <$ symbol "/"), InfixL (Modulo <$ symbol "%")],
  [InfixL (Sum <$ symbol "+"), InfixL (Subtr <$ symbol "-")]
  ]

numericalExprParser :: Parser NumExpr
numericalExprParser = makeExprParser pTerm operatorTable

-- Instructions --
------------------

newtype Ident = Ident String deriving (Eq, Ord, Show)
parseIdent ::  Parser Ident
parseIdent =  Ident <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

data Instr = 
  Assign Ident NumExpr
  | If BoolExpr Instr (Maybe Instr)
  | Goto Ident
  | Gosub Ident
  | Block [Instr]
  | Output NumExpr
  | Input Ident
  | Tag Ident Instr
  | Return
  deriving (Eq, Ord, Show)

-- nie ma za duzo o tym jak ma dzialac return. Wiec zakladam, ze jest to zwykla instrukcja
parseInstr :: Parser Instr
parseInstr =
  parseGoto
  <|> parseOutput
  <|> parseInput
  <|> parseReturn
  <|> try parseTag
  <|> try parseAssignment 
  <|> parseIf
  <|> parseBlock

parseReturn :: Parser Instr
parseReturn = symbol "return" >> return Return

parseAssignment :: Parser Instr
parseAssignment = do
  IdentN ident <- parseIdentN
  _ <- symbol ":="
  ne <- numericalExprParser
  return $ Assign (Ident ident) ne

parseBlock :: Parser Instr
parseBlock = do
  _ <- symbol "begin"
  b <- many parseSerialInstr
  _ <- symbol "end"
  return $ Block b

parseSerialInstr :: Parser Instr 
parseSerialInstr = do
  instr <- parseInstr
  _ <- symbol ";"
  return instr

parseOutput :: Parser Instr
parseOutput = symbol "print" >> Output <$> numericalExprParser

parseInput :: Parser Instr
parseInput = symbol "read" >> Input <$> parseIdent

parseTag:: Parser Instr
parseTag= do
  ident <- parseIdent
  _ <- symbol ":"
  ne <- parseInstr 
  return $ Tag ident ne

parseIf :: Parser Instr
parseIf = do
  _ <- symbol "if"
  be <- parseBoolExpr
  _ <- symbol "then"
  instr <- parseInstr
  optional_else <- optional (symbol "else" >> parseInstr)
  return $ If be instr optional_else

parseGoto :: Parser Instr
parseGoto = choice 
  [symbol "goto" >> Goto <$> parseIdent,
   symbol "gosub" >> Gosub <$> parseIdent]

data Decl = 
  LabelDecl Ident
  | VarDecl Ident
  deriving (Eq, Ord, Show)

parseLabelDecl :: Parser Decl
parseLabelDecl = LabelDecl <$> (symbol "label" >> parseIdent)

parseVarDecl:: Parser Decl
parseVarDecl= VarDecl<$> (symbol "var" >> parseIdent)

parseDeclaration :: Parser Decl
parseDeclaration = do
  c <- choice [parseLabelDecl, parseVarDecl]
  _ <- symbol ";"
  return c

parseDeclarations :: Parser [Decl]
parseDeclarations = many parseDeclaration

parseProgram :: Parser ([Decl], [Instr])
parseProgram = do
  decls <- parseDeclarations
  instrs <- many parseSerialInstr
  return (decls, instrs)


parseProgramInstr :: Parser [Instr]
parseProgramInstr = do
  _ <- parseDeclarations
  instrs <- many parseSerialInstr
  return instrs

parseProgramDecl :: Parser [Decl]
parseProgramDecl = do
  decls <- parseDeclarations
  _ <- many parseSerialInstr
  return decls

sampleProgram :: Text
sampleProgram = "label loop; var k;\
            \var j; \
            \var i; \
            \read k; \
            \if k > 0 then begin \
            \j:=1; i:=k; loop: \
            \j:=j*i; \
            \i:=i-1; \
            \if i > 1 then goto loop; \
            \ print j; \
            \end;"