{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Replace case with maybe" #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use replicate" #-}
{-# HLINT ignore "Use zipWith" #-}

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
    compileRelationalExpr,
    defaultContext,
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
import qualified Data.Text.IO as Data.Text
import Data.Tuple (swap)

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
  | Exit
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

parseExit :: Parser Instr
parseExit = symbol "exit" >> return Exit

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

compile :: ([Decl], [Instr]) -> (Text, CompilationContext)
compile (declarations, instructions) = (output, config3)
  where
    (text, initialConfig) = compileDecl declarations
    (text2, config1) = foldl (\(text, config) instr -> let (newText, newConfig) = compileInstr config instr in (text <> newText, newConfig)) (text, initialConfig) instructions
    (stage2C,config2) = stage2 (text2, config1)
    (backPatchedCode, config3) = backpatch (stage2C, config2)
    output = foldl (<>) (pack "") (intersperse (pack "\n") backPatchedCode)
    -- newlined = intersperse (pack "\n") (fst x)
    -- y = foldl (<>) (pack "") newlined

readAndCompile :: IO ()
readAndCompile = do
  input <- readFile "./test/testfile1.txt" 
  let parsed = parse parseProgram "" (pack input)
  case parsed of 
    Left err -> print err
    Right o ->  do
      let (t,ct) = compile o
      printContext ct
      Data.Text.putStrLn t

compileConstNum :: CompilationContext-> NumExpr -> ([Text],CompilationContext)
compileConstNum i n = case n of
  ConstNum x -> ([ pack $ show i ++ "\tpush " ++ show x ], Lib.succ i )
  UnaryPlus x -> ([ pack $ show i ++ "\tpush " ++ show x ], Lib.succ i )
  _ -> error "Expected constNum here, got something else"

compileUnaryPlus :: CompilationContext -> NumExpr -> ([Text],CompilationContext)
compileUnaryPlus = compileConstNum

data CompilationContext = CompilationContext {
  instrCount :: Int,
  labelMap:: [(Int, String)],
  varMap :: [String]
} deriving (Eq)
succ :: CompilationContext -> CompilationContext
succ c = c { instrCount = Prelude.succ $ instrCount c }
instance Show CompilationContext where
  show c = show $ instrCount c

instance Enum CompilationContext where
  toEnum :: Int -> CompilationContext
  toEnum = undefined
  fromEnum :: CompilationContext -> Int
  fromEnum = undefined

instance Num CompilationContext where
  (+) c1 c2 = CompilationContext {
    instrCount = instrCount c1 + instrCount c2,
    labelMap= labelMap c1 ++ labelMap c2,
    varMap = varMap c1 ++ varMap c2
  }
  (*) c1 c2 = CompilationContext {
    instrCount = instrCount c1 * instrCount c2,
     labelMap= labelMap c1 ,
    varMap = varMap c1 ++ varMap c2
  }
  abs c = c
  signum c = c
  fromInteger i = CompilationContext {
    instrCount = fromInteger i,
    labelMap= [],
    varMap = []
  }
  negate c = c

getAddressOfVariable :: CompilationContext -> String -> Maybe Int
getAddressOfVariable cc s =
   case elemIndex s $ varMap cc of
    Just i ->  Just i
    Nothing -> Nothing

printContext :: CompilationContext -> IO ()
printContext c = do
  putStrLn "##### CompilationContext #####"
  putStrLn $ "instrCount: " ++ show (instrCount c)
  putStrLn $ "labelMap: " ++ show (labelMap c)
  putStrLn $ "varMap: " ++ show (varMap c)
  putStrLn ""
data BindOp = ConstNum_ | IdentN_
compileBindOp :: BindOp -> CompilationContext-> NumExpr -> ([Text],CompilationContext)
compileBindOp o i b = ([sumLine], Lib.succ i)
  where
    sumLine = pack (show i) <> bindOp o
    operation = case b of
      ConstNum x -> show x
      IdentN x -> case getAddressOfVariable i x of
        Just x -> show x
        _ -> "@" <> x  
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

data BinaryOp = Sum_ | Subtr_ | Product_ | Division_ | Modulo_
compileBinaryOp :: BinaryOp -> CompilationContext -> NumExpr -> NumExpr -> ([Text],CompilationContext)
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
-- refactor this, it's supposed to save stuff to memory.
  where
    (ident, ne) = case i of
      Assign x ne -> (x, ne)
      _ -> error "Expected assignment here, got something else"
    Ident identKey = ident
    (progNe, ctx1 ) = compileNumExpr cc ne
    address = case getAddressOfVariable ctx1 identKey of
      Just a -> show a
      _ -> "@" <> identKey 
    sumLine = pack (show ctx1) <> "\tPOP $" <> pack  address
    sumProgram = progNe ++ [sumLine]


compileRelationalExpr :: CompilationContext -> RelationalExpr -> ([Text], CompilationContext)
compileRelationalExpr cc re = (finalInstructions, finalContext)
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
    instructions = pack <$> ["PUSH " ++ show addressToPutInReturn, "POP $" ++ show addressOfReturn , "SUB"] ++ condition ++ ["JMP @setFalse"]
    instr = map countInstr (zip [instrCount n2InstrCount..] instructions )
    finalInstructions = n1Program <> n2Program <> instr
    finalContext = foldl (\ctx i -> Lib.succ ctx) n2InstrCount instructions

countInstr :: (Int , Text) -> Text
countInstr (num, instr) =  (pack . show) num <> "\t" <> instr

-- a b
-- push a push b
-- [a b]
-- mnozenie
-- [a*b]

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

compileInput :: CompilationContext -> Ident -> ([Text], CompilationContext)
compileInput cc (Ident ident) = (instrs, Lib.succ $ Lib.succ cc)
  where 
    address = case  getAddressOfVariable cc ident of
      Just a -> show a
      _ -> error "Variable not found"
    f = \(instr, ctx) -> pack $ show ctx <> "\t" <> instr
    instrs = map f (zip [ "READ", "POP $" <> address ] (iterate Lib.succ cc))

compileIOandCtrl :: CompilationContext -> Instr -> ([Text], CompilationContext)
compileIOandCtrl cc i= ([pack instr], Lib.succ cc)
  where
    (ioType) = case i of
      Output _ -> "PRINT"
      Exit ->   "STOP"
      _ -> error "Expected IO instruction here, got something else"
    instr = show cc <> "\t" <> ioType

compileBlock :: CompilationContext -> Instr -> ([Text], CompilationContext)
compileBlock cc i = (instrs, ctx)
  where
    instructions = case i of
      Block x-> x
      _ -> error "Expected block here, got something else"
    (instrs, ctx) = foldl (\(instrs, ctx) i -> let (instr, ctx1) = compileInstr ctx i in (instrs ++ instr, ctx1)) ([], cc) instructions

data BinaryBoolExpr = And_ | Or_
compileBinaryBoolExpr :: BinaryBoolExpr -> CompilationContext -> BoolExpr -> BoolExpr -> ([Text], CompilationContext)
compileBinaryBoolExpr t cc be1 be2 = (fullInstr, fullCtx)
  where
    (program1, ctx1) = compileBoolExpr cc be1
    (program2, ctx2) = compileBoolExpr ctx1 be2
    binInstr = case t of
      And_ -> "MUL"
      Or_ -> "ADD"
      _ -> error "Expected binary bool expr here, got something else"
    fullInstr = program1 ++ program2 ++ (pack <$> [ show ctx2 <> "\t" <> binInstr, show (Lib.succ ctx2) <> "\tJNZ @setTrue", show (Lib.succ $ Lib.succ ctx2) <> "\tJMP @setFalse"])
    fullCtx = Lib.succ $ Lib.succ ctx2
    -- bug - is True and False 0 and 1 or -1 and 1?

compileBoolNot :: CompilationContext -> BoolExpr -> ([Text], CompilationContext)
compileBoolNot cc be = (fullInstr, finalContext)
  where
    (program, ctx) = compileBoolExpr cc be
    negationInstr = ["PUSH -1", "add", "neg"]
    numberedConverstionInstr = fmap (\(instr, ct) -> pack $ show ct <> "\t" <> instr) (zip negationInstr (show <$> [ctx..]))
    fullInstr = program ++ numberedConverstionInstr
    finalContext = iterate Lib.succ ctx !! length negationInstr

compileConstBool :: CompilationContext -> BoolExpr -> ([Text], CompilationContext)
compileConstBool ctx be = ([instr], Lib.succ ctx)
  where
    val = case be of
      ConstBool x -> if x then "1" else "0"
      _ -> error "Expected const bool here, got something else"
    instr = pack $ show ctx <> "\tPUSH " <> val

compileBoolIdent :: CompilationContext -> BoolExpr -> ([Text], CompilationContext)
compileBoolIdent ctx be = ([instr], Lib.succ ctx)
  where
    val = case be of
      IdentB x -> x
      _ -> error "Expected const bool here, got something else"
    address = case getAddressOfVariable ctx val of
      Just a -> a
      _ -> error "Variable not found"
    instr = pack $ show ctx <> "\tPUSH $" <> val

compileBoolExpr :: CompilationContext -> BoolExpr -> ([Text], CompilationContext)
compileBoolExpr cc be = case be of
  ConstBool _ -> compileConstBool cc be
  IdentB _ -> compileBoolIdent cc be
  Not _ -> compileBoolNot cc be
  And be1 be2 -> compileBinaryBoolExpr And_ cc be1 be2
  Or be1 be2 -> compileBinaryBoolExpr Or_ cc be1 be2
  Relational _ -> compileRelationalExpr cc rel
    where rel = case be of
            Relational x -> x
            _ -> error "Expected relational expr here, got something else"
  _ -> error "Expected bool expr here, got something else"

compileIf :: CompilationContext -> BoolExpr -> Instr -> Maybe Instr -> ([Text], CompilationContext)
compileIf cc be firstForm optionalSecondForm =
  case optionalSecondForm of
    Just secondForm -> compileIfElse cc be firstForm secondForm
    Nothing -> compileRawIf cc be firstForm

compileRawIf :: CompilationContext -> BoolExpr -> Instr -> ([Text], CompilationContext)
compileRawIf cc be i = (fullProgram, ctxAfterInstr)
  where
    (bEProgram, ctxAfterBE) = compileBoolExpr cc be
    (instrProgram, ctxAfterInstr) = compileInstr (Lib.succ ctxAfterBE) i
    jumpToTheEnd::[Text]
    jumpToTheEnd = [ pack $ show  ctxAfterBE <> "\tJZ $" <> show ctxAfterInstr]
    fullProgram = bEProgram <> jumpToTheEnd <> instrProgram

compileIfElse :: CompilationContext -> BoolExpr -> Instr -> Instr -> ([Text], CompilationContext)
compileIfElse cc be a b = (fullProgram, ctxAfterInstrB)
  where
    (bEProgram, ctxAfterBE) = compileBoolExpr cc be
    (instrProgramA, ctxAfterInstrA) = compileInstr (Lib.succ ctxAfterBE) a
    (instrProgramB, ctxAfterInstrB) = compileInstr (Lib.succ ctxAfterInstrA) b
    jumpConditional::[Text]
    jumpConditional = [ pack $ show ctxAfterBE <> "\tJZ $" <> show positionB]
    jumpToFinish::[Text]
    jumpToFinish = [ pack $ show ctxAfterInstrA <> "\tJMP $" <> show ctxAfterInstrB]
    positionB :: CompilationContext
    positionB = Lib.succ ctxAfterInstrA
    fullProgram = bEProgram <> jumpConditional <> instrProgramA <> jumpToFinish <> instrProgramB

compileTag :: CompilationContext -> Ident -> Instr -> ([Text], CompilationContext)
compileTag cc id instr = (prog, newCtx)
  where
    idname = case id of 
      Ident x -> x
      _ -> error "Expected ident here, got something else"
    (prog, ca) = compileInstr cc instr
    newCtx = CompilationContext{
      instrCount = instrCount ca,
      labelMap = (instrCount cc, idname) : labelMap ca,
      varMap = varMap ca
    }


compileGoto :: CompilationContext -> Ident -> ([Text], CompilationContext)
compileGoto cc i = ([instr], Lib.succ cc)
  where
    str = case i of
      Ident x -> x
      _ -> error "Expected ident here, got something else"
    address = case getAddressOfLabel cc str of
      Just a -> show a
      _ -> "@" <> str
    instr = pack $ show cc <> "\tJMP " <> address

compileGosub :: CompilationContext -> Ident -> ([Text], CompilationContext)
compileGosub cc i = (instr, finalCtx)
  where
    str = case i of
      Ident x -> x
      _ -> error "Expected ident here, got something else"
    address = case getAddressOfLabel cc str of
      Just a -> show a
      _ -> "@" <> str
    finalCtx = iterate Lib.succ cc !! 2
    instr = pack <$> [ show cc <> "\tPUSH " <> show finalCtx , show (Lib.succ cc) <> "\tJMP " <> address]

compileReturn :: CompilationContext -> ([Text], CompilationContext)
compileReturn cc = (instr,finalCtx)
  where
    instr = pack <$> ( map (\(i,c) -> show c <> "\t" <> i) $ zip [ "pop $" <> returnRegister, "jmp" ] (iterate Lib.succ cc))
    finalCtx = iterate Lib.succ cc !! length instr
    returnRegister = case getAddressOfVariable cc "ret" of
      Just a -> show a
      _ -> error "Variable ret not found"

compileInstr :: CompilationContext -> Instr -> ([Text], CompilationContext)
compileInstr cc instr =
  case instr of
    Assign _ _ -> compileAssignment cc instr
    If be i1 i2 -> compileIf cc be i1 i2
    Goto i -> compileGoto cc i
    Gosub i -> compileGosub cc i
    Block _ -> compileBlock cc instr
    Output _ -> compileIOandCtrl cc instr
    Input i -> compileInput cc i
    Tag i ins -> compileTag cc i ins
    Return -> compileReturn cc
    Exit -> compileIOandCtrl cc instr

compileDecl :: [ Decl ] -> ([Text], CompilationContext)
compileDecl decl = ([dataLine], newCtx)
  where
    labels = filter isALabel decl
    variables = filter (not . isALabel) decl
    newCtx = defaultContext { varMap = varMap defaultContext ++  (unwrapDecl <$> variables) }
    dataLine::Text
    dataLine = pack $ "DATA " ++ show (Data.List.intersperse ',' $ take (length variables) $ repeat '0')
    unwrapDecl :: Decl -> String
    unwrapDecl d = case d of
      LabelDecl (Ident x) -> x
      VarDecl (Ident x) -> x 

defaultContext :: CompilationContext
defaultContext = CompilationContext {
  instrCount = 0,
  labelMap = [],
  varMap = ["ret", "boolSetRet"]
}

stage2 :: ([Text], CompilationContext) -> ([Text], CompilationContext)
stage2 (txs, cc) =  (boolScaff, cc2)
  where
    endLine = show cc <> "\tSTOP"
    (boolScaff, cc2) = insertBooleanScaffolding (txs <> ( pack <$> [endLine]), Lib.succ cc)

insertBooleanScaffolding :: ([Text], CompilationContext) -> ([Text], CompilationContext)
insertBooleanScaffolding (t, cc) = (t <> blText ,finalCtx)
  where
    (blText, cc2) = booleanLabels cc  
    setTrueAddr = cc
    setFalseAddr = iterate Lib.succ cc !! 3
    finalCtx = CompilationContext {
      instrCount = instrCount cc2,
      labelMap = labelMap cc2 ++ [ (instrCount setTrueAddr, "setTrue"), (instrCount setFalseAddr, "setFalse") ],
      varMap = varMap cc2 
    }
booleanLabels :: CompilationContext -> ([Text], CompilationContext)
booleanLabels cc = ([pack "#BoolLabels"] <> numbered, countedCtx)
  where
    returnAddress = case getAddressOfVariable cc "boolSetRet" of
      Just a -> show a
      _ -> error "Variable ret not found"
    content = ["POP", "PUSH 1", "JMP "<>returnAddress, "POP", "PUSH 0", "JMP " <> returnAddress]
    numbered = zipWith (\x y -> pack (show x) <> "\t" <> y) [instrCount cc..] $ pack <$> content
    countedCtx = Prelude.foldl (\x y -> Lib.succ x) cc content

parseHole :: Parser Text
parseHole = do
  _ <- many alphaNumChar
  _ <- many (letterChar <|> char ' ' <|> char '\t')
  _ <- symbol "@"
  holeName <- lexeme ((:) <$> letterChar <*> many alphaNumChar)
  return $ pack holeName

patchHole :: CompilationContext -> Text -> Text
patchHole cc l = if hasHole then patchedLine else l 
  where
    (hasHole, hName) = case parse parseHole "" l of
      Left _ -> (False, "")
      Right holeName -> (True, holeName)
    patchedLine = TextPack.replace ("@" <> hName) (pack addr) l
    addr = case getAddressOfLabel cc (TextPack.unpack hName) of
      Just a -> show a
      _ -> error ("Label " <> TextPack.unpack hName <>" not found")
    
backpatch :: ([Text], CompilationContext) -> ([Text], CompilationContext)
backpatch (lines, cc) = (patchedLines, cc)
  where
    patchedLines = patchHole cc <$> lines
    

testContext = CompilationContext {
  instrCount = 0,
  labelMap = [(21, "setTrue"), (24, "setFalse")],
  varMap = []
}

getAddressOfLabel :: CompilationContext -> String -> Maybe Int
getAddressOfLabel cc idn = lookup idn $ fmap swap (labelMap cc)