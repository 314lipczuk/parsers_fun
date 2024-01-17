{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Replace case with maybe" #-}

module Lib
  ( Decl (..),
    parseLabelDecl,
    parseProgramInstr,
    parseProgramDecl,
    parseVarDecl,
    parseDeclaration,
    parseDeclarations,
    parseProgram,
    parseBlock,
    Ident (..),
    parseSerialInstr,
    parseInput,
    parseOutput,
    parseTag,
    parseIf,
    parseGoto,
    parseInstr,
    parseBoolExpr,
    parseNum,
    parseAssignment,
    Instr (..),
    BoolExpr (..),
    NumExpr (..),
    RelationalExpr (..),
    compileNumExpr,
    CompilationContext(..)
  )
where

import Control.Monad.Combinators.Expr
import qualified Data.Text as TextPack
import Data.Text (pack)
import Data.Char (isAlpha)
import Data.Functor (($>), (<&>))
import Data.Text (Text)
import Data.Void
import GHC.Conc (par)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (elemIndex)
import Data.List (intersperse)
import Data.Text.Lazy (unpack)
import qualified Data.Text.IO as Data.Text

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Text -> Parser Text
symbol = L.symbol space

-- Boolean expressions  --
--------------------------

data BoolExpr
  = ConstBool Bool
  | IdentB String
  | Not BoolExpr
  | And BoolExpr BoolExpr
  | Or BoolExpr BoolExpr
  | Relational RelationalExpr
  deriving (Eq, Ord, Show)

operatorTableBool :: [[Operator Parser BoolExpr]]
operatorTableBool =
  [ [Prefix (Not <$ symbol "not")],
    [InfixL (And <$ symbol "and")],
    [InfixL (Or <$ symbol "or")]
  ]

parseBoolExpr :: Parser BoolExpr
parseBoolExpr = makeExprParser bTerm operatorTableBool

bTerm :: Parser BoolExpr
bTerm = choice [parseConstBool, parseRelationalExpr <&> Relational, parseIdentB, parens parseBoolExpr]

parseConstBool :: Parser BoolExpr
parseConstBool = ConstBool <$> (True <$ symbol "true" <|> False <$ symbol "false")

parseIdentB :: Parser BoolExpr
parseIdentB = IdentB <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

-- Relational expressions --
----------------------------
data RelationalExpr
  = GreaterEquals NumExpr NumExpr
  | Greater NumExpr NumExpr
  | LessEquals NumExpr NumExpr
  | Less NumExpr NumExpr
  | Equals NumExpr NumExpr
  | NotEquals NumExpr NumExpr
  deriving (Eq, Ord, Show)

parseRelationalExpr :: Parser RelationalExpr
parseRelationalExpr = do
  ne1 <- numericalExprParser
  op <- symbol ">=" <|> symbol ">" <|> symbol "<=" <|> symbol "<" <|> symbol "=" <|> symbol "<>"
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

data NumExpr
  = ConstNum Int
  | IdentN String
  | Negation NumExpr
  | UnaryPlus NumExpr
  | Sum NumExpr NumExpr
  | Subtr NumExpr NumExpr
  | Product NumExpr NumExpr
  | Division NumExpr NumExpr
  | Modulo NumExpr NumExpr
  deriving (Eq, Ord, Show)

parseConstNum :: Parser NumExpr
parseConstNum = ConstNum <$> lexeme L.decimal

parseIdentN :: Parser NumExpr
parseIdentN = IdentN <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser NumExpr
pTerm = choice [parens numericalExprParser, parseConstNum, parseIdentN]

operatorTable :: [[Operator Parser NumExpr]]
operatorTable =
  [ [Prefix (Negation <$ symbol "-"), Prefix (UnaryPlus <$ symbol "+")],
    [InfixL (Product <$ symbol "*"), InfixL (Division <$ symbol "/"), InfixL (Modulo <$ symbol "%")],
    [InfixL (Sum <$ symbol "+"), InfixL (Subtr <$ symbol "-")]
  ]

numericalExprParser :: Parser NumExpr
numericalExprParser = makeExprParser pTerm operatorTable

-- Instructions --
------------------

newtype Ident = Ident String deriving (Eq, Ord, Show)

parseIdent :: Parser Ident
parseIdent = Ident <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

data Instr
  = Assign Ident NumExpr
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

parseTag :: Parser Instr
parseTag = do
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
parseGoto =
  choice
    [ symbol "goto" >> Goto <$> parseIdent,
      symbol "gosub" >> Gosub <$> parseIdent
    ]

data Decl
  = LabelDecl Ident
  | VarDecl Ident
  deriving (Eq, Ord, Show)

parseLabelDecl :: Parser Decl
parseLabelDecl = LabelDecl <$> (symbol "label" >> parseIdent)

parseVarDecl :: Parser Decl
parseVarDecl = VarDecl <$> (symbol "var" >> parseIdent)

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


-- Compilation --

isALabel :: Decl -> Bool
isALabel x = case x of
  (LabelDecl _) -> True
  _ -> False

compile :: ([Decl], [Instr]) -> Text
compile (declarations, instructions) = undefined
  where
    labels = filter isALabel declarations
    variables = filter (not . isALabel) declarations

transform :: (Instr, Int) -> (Text, Int)
transform = undefined

compileConstNum :: CompilationContext-> NumExpr -> ([Text],CompilationContext)
compileConstNum i n = case n of
  ConstNum x -> ([ pack $ show i ++ "\tpush " ++ show x ], Lib.succ i )
  UnaryPlus x -> ([ pack $ show i ++ "\tpush " ++ show x ], Lib.succ i )
  _ -> error "Expected constNum here, got something else"

compileUnaryPlus :: CompilationContext -> NumExpr -> ([Text],CompilationContext)
compileUnaryPlus = compileConstNum 

data CompilationContext = CompilationContext {
  instrCount :: Int,
  varCount :: Int,
  varMap :: [String]
} deriving (Eq)
succ :: CompilationContext -> CompilationContext
succ c = c { instrCount = Prelude.succ $ instrCount c }
instance Show CompilationContext where
  show c = show $ instrCount c
instance Num CompilationContext where
  (+) c1 c2 = CompilationContext {
    instrCount = instrCount c1 + instrCount c2,
    varCount = varCount c1 + varCount c2,
    varMap = varMap c1 ++ varMap c2
  }
  (*) c1 c2 = CompilationContext {
    instrCount = instrCount c1 * instrCount c2,
    varCount = varCount c1 * varCount c2,
    varMap = varMap c1 ++ varMap c2
  }
  abs c = c
  signum c = c
  fromInteger i = CompilationContext {
    instrCount = fromInteger i,
    varCount = fromInteger i,
    varMap = []
  }
  negate c = c

getAddressOfVariable :: CompilationContext -> String -> Maybe Int
getAddressOfVariable cc s = 
  case elemIndex s $ varMap cc of
    Just i -> Just i
    Nothing -> Nothing

data BindOp = ConstNum_ | IdentN_
compileBindOp :: BindOp -> CompilationContext-> NumExpr -> ([Text],CompilationContext)
compileBindOp o i b = ([sumLine], Lib.succ i)
  where
    sumLine = pack (show i) <> bindOp o 
    operation = case b of
      ConstNum x -> show x
      IdentN x -> case getAddressOfVariable i x of
        Just x -> show x
        _ -> error "Variable not found"
      _ -> error "Expected constNum here, got something else"
    bindOp :: BindOp -> Text
    bindOp o = pack $ (<>) "\t" $ case o of
      ConstNum_ -> "PUSH " <> operation
      IdentN_ -> "PUSH $" <> operation

data UnaryOp = UnaryPlus_ | Negation_ 
compileUnaryOp :: UnaryOp -> CompilationContext -> NumExpr -> ([Text], CompilationContext)
compileUnaryOp o i n = (sumProgram, Lib.succ ctx)
  where 
    (nProgram,ctx) = compileNumExpr i unwrapped
    unwrapped = case n of
      UnaryPlus x -> x
      Negation x -> x
      _ -> error "Expected unaryPlus here, got something else"
    sumLine = pack ( show ctx) <> unaryOp o
    sumProgram = nProgram ++ [sumLine]
    unaryOp :: UnaryOp -> Text
    unaryOp o = pack $ (<>) "\t" $ case o of
      UnaryPlus_ -> "NOP"
      Negation_ -> "NEG"

data BinaryOp = Sum_ | Subtr_ | Product_| Division_ | Modulo_
compileBinaryOp :: BinaryOp -> CompilationContext-> NumExpr -> NumExpr -> ([Text],CompilationContext)
compileBinaryOp o i n1 n2 = (sumProgram, Lib.succ n2InstrCount)
  where (n1Program, n1InstCount) = compileNumExpr i n1
        (n2Program, n2InstrCount) = compileNumExpr n1InstCount n2 
        sumLine = pack ( show n2InstrCount) <> binaryOp o
        sumProgram = n1Program ++ n2Program ++ [sumLine]
        binaryOp :: BinaryOp -> Text
        binaryOp o = pack $ (<>) "\t" $ case o of
          Sum_ -> "ADD"
          Subtr_ -> "SUB"
          Product_ -> "MUL"
          Division_ -> "DIV"
          Modulo_ -> "MOD" 
    
compileNumExpr :: CompilationContext -> NumExpr -> ([Text],CompilationContext) 
compileNumExpr i e = case e of
  ConstNum _ -> compileBindOp ConstNum_ i e
  IdentN _ -> compileBindOp IdentN_ i e
  Negation _ -> compileUnaryOp Negation_ i e
  UnaryPlus _ -> compileUnaryOp UnaryPlus_ i e
  Sum n1 n2 -> compileBinaryOp Sum_ i n1 n2
  Subtr n1 n2 -> compileBinaryOp Subtr_ i n1 n2
  Product n1 n2 -> compileBinaryOp Product_ i n1 n2
  Division n1 n2 -> compileBinaryOp Division_ i n1 n2
  Modulo n1 n2 -> compileBinaryOp Modulo_ i n1 n2
  _ -> error "Not implemented"

printCompilationResult :: ([Text], a) -> IO ()
printCompilationResult x = Data.Text.putStrLn y 
  where
    newlined = intersperse (pack "\n") (fst x)
    y = foldl (<>) (pack "") newlined


compileAssignment :: CompilationContext -> Instr -> ([Text], CompilationContext)
compileAssignment cc i = (sumProgram, Lib.succ ctx1)
  where 
    (ident, ne) = case i of  
      Assign x ne -> (x, ne)
      _ -> error "Expected assignment here, got something else"
    Ident identKey = ident
    (progNe, ctx1 ) = compileNumExpr cc ne
    address = case  getAddressOfVariable ctx1 identKey of
      Just a -> a
      _ -> error "Variable not found"
    
    sumLine = pack (show ctx1) <> "\tPOP $" <> pack (show address)
    sumProgram = progNe ++ [sumLine]

compileIf :: CompilationContext -> BoolExpr -> Instr -> Maybe Instr -> ([Text], CompilationContext)
compileIf = undefined

compileBoolExpr :: CompilationContext -> BoolExpr -> ([Text], CompilationContext)
compileBoolExpr = undefined

compileRelationalExpr :: CompilationContext -> RelationalExpr -> ([Text], CompilationContext)
compileRelationalExpr cc re = (instructions, finalContext)
  where 
    (n1, n2, condition) = case re of 
      GreaterEquals n1 n2 -> (n1, n2, ["PUSH 1", "SUB", "JGZ @setTrue"])
      Greater n1 n2 -> (n1, n2, ["JGZ @setTrue"])
      LessEquals n1 n2 -> (n1, n2, ["PUSH 1", "SUB", "JLZ @setTrue" ] )
      Less n1 n2 -> (n1, n2, [ "JLZ @setTrue" ])
      Equals n1 n2 -> (n1, n2, ["JZ @setTrue"])
      NotEquals n1 n2 -> (n1, n2, ["JNZ @setTrue"])
      _ -> error "Expected relational expression here, got something else"
    (n1Program, n1InstCount) = compileNumExpr cc n1 
    (n2Program, n2InstrCount) = compileNumExpr n1InstCount n2 
    addressToPutInReturn = instrCount n2InstrCount + length condition + 4
    addressOfReturn = case getAddressOfVariable n2InstrCount "ret" of 
      Just a -> a
      _ -> error "Variable ret not found"
    instructions = pack <$> ["PUSH " ++ show addressToPutInReturn, "POP" ++ show addressOfReturn , "SUB"] ++ condition ++ ["JMP @setFalse"]
    finalContext = foldl (\ctx i -> Lib.succ ctx) n2InstrCount instructions 

-- GreaterEq -> sub -> push 1 -> sub -> jgz
-- Greater -> sub -> jgz
-- Less-> sub -> jlz
-- LessEq-> sub -> push 1 -> sub -> jlz
-- Eq -> sub -> jz
-- Neq -> sub -> jnz

-- in reg: 13

-- 12 push 1
-- 13 PUSH retaddr
-- 14 POP retaddr
-- 15 SUB
-- 16 JGZ | 16 PUSH 1 -> 17 SUB -> 18 JGZ setTrue
--15|19 JMP setFalse
-- # after jump, should have 1 or 0 on stack as true or false
-- 18 | 20 nop

-- wiec zeby skoczyć dobrze, muszę dać $CURRENT + LEN(DYNAMIC) + 4

booleanLabels :: CompilationContext -> ([Text], CompilationContext)
booleanLabels cc = ([pack "#BoolLabels"] <> numbered, countedCtx)
  where
    content = ["POP", "PUSH 1", "JMP @ret", "POP", "PUSH 0", "JMP @ret"] 
    numbered = zipWith (\x y -> pack (show x) <> "\t" <> y) [instrCount cc..] content
    countedCtx = Prelude.foldl (\x y -> Lib.succ x) cc content

defaultContext :: CompilationContext
defaultContext = CompilationContext {
  instrCount = 0,
  varCount = 0,
  varMap = ["ret"]
}