{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (isPrefixOf, intersperse)
import Lib
import Test.Hspec
import Test.Tasty.Hspec
import Text.Megaparsec (eof, parse)
import Lib (compileNumExpr, CompilationContext(..))
import Data.Text (pack)

main :: IO ()
main = hspec spec_parseBoolExpr

spec_parseBoolExpr :: Spec
spec_parseBoolExpr = do
  it "parses true" $
    case parse parseBoolExpr "" "true" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` ConstBool True

  it "parses false" $
    case parse parseBoolExpr "" "false" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` ConstBool False

  it "parses simple and expression" $
    case parse parseBoolExpr "" "true and false" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` And (ConstBool True) (ConstBool False)

  it "parses simple or expression" $
    case parse parseBoolExpr "" "true or false" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Or (ConstBool True) (ConstBool False)

  it "parses relational expression: 1+1 > -1" $
    case parse parseBoolExpr "" "1+1 > -1" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (Negation (ConstNum 1)))

  it "parses relational expression with bool: 1+1 > -1 or false" $
    case parse parseBoolExpr "" "1+1 > -1 or false" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Or (Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (Negation (ConstNum 1)))) (ConstBool False)

  it "doesnt parse only numerical expression" $
    case parse parseBoolExpr "" "1+1" of
      Left err -> ("ParseErrorBundle" `isPrefixOf` show err) `shouldBe` True
      Right result -> fail "Should not parse, but got:"

  it "should not parse nothing" $
    case parse parseBoolExpr "" "" of
      Left err -> ("ParseErrorBundle" `isPrefixOf` show err) `shouldBe` True
      Right result -> fail "Should not parse nothing"

  it "should parse assignment" $
    case parse parseAssignment "" "x := 1+8" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Assign (Ident "x") (Sum (ConstNum 1) (ConstNum 8))
  it "should parse output" $
    case parse parseOutput "" "print 1 + abc21 * 7" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Output (Sum (ConstNum 1) (Product (IdentN "abc21") (ConstNum 7)))

  it "should parse input" $
    case parse parseInput "" "read abc21" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Input (Ident "abc21")

  it "should parse tag" $
    case parse parseTag "" "loop: goto start" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Tag (Ident "loop") (Goto (Ident "start"))

  it "should parse simple If" $
    case parse parseIf "" "if 1 + 1 > a then print 0" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` If (Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (IdentN "a"))) (Output (ConstNum 0)) Nothing

  it "should parse complex If" $
    case parse parseIf "" "if 1 + 1 > a then print 0 else print 1" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` If (Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (IdentN "a"))) (Output (ConstNum 0)) (Just (Output (ConstNum 1)))

  -- the same as above, but with full instruction parser
  -- this checks if precedence isnt broken
  it "should parse assignment as instruction" $
    case parse parseInstr "" "x := 1+8" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Assign (Ident "x") (Sum (ConstNum 1) (ConstNum 8))
  it "should parse output as instruction" $
    case parse parseInstr "" "print 1 + abc21 * 7" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Output (Sum (ConstNum 1) (Product (IdentN "abc21") (ConstNum 7)))

  it "should parse input as instruction" $
    case parse parseInstr "" "read abc21" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Input (Ident "abc21")

  it "should parse tag as instruction" $
    case parse parseInstr "" "loop: goto start" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` Tag (Ident "loop") (Goto (Ident "start"))

  it "should parse simple If as instruction" $
    case parse parseInstr "" "if 1 + 1 > a then print 0" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` If (Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (IdentN "a"))) (Output (ConstNum 0)) Nothing

  it "should parse complex If as instruction" $
    case parse parseInstr "" "if 1 + 1 > a then print 0 else print 1" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` If (Relational (Greater (Sum (ConstNum 1) (ConstNum 1)) (IdentN "a"))) (Output (ConstNum 0)) (Just (Output (ConstNum 1)))

  it "should parse complex If with simple relation" $
    case parse parseProgramInstr "" "if 1 > a then print 0 else print 1;" of
      Left err -> fail (show err)
      Right result -> result `shouldBe` [If (Relational (Greater (ConstNum 1) (IdentN "a"))) (Output (ConstNum 0)) (Just (Output (ConstNum 1)))]

  it "compile simple numerical expression: 1 + a" $ do
    let expr = Sum (ConstNum 1) (IdentN "a")
    let compilationContext = CompilationContext {instrCount = 0, varCount= 0, varMap=["a"]}
    let expected = (pack <$> ["0\tPUSH 1", "1\tPUSH $0", "2\tADD"],3)
    let got = compileNumExpr compilationContext expr
    fst got `shouldBe` fst expected
    let showCC = show . snd
    showCC got `shouldBe` showCC expected

  it "compile more complex numerical expression: -(1+a) * 4" $ do
    let expr = Product (Negation(Sum (ConstNum 1) (IdentN "a") )) (ConstNum 4) 
    let compilationContext = CompilationContext {instrCount = 0, varCount= 0, varMap=["a"]}
    let expected = (pack <$> ["0\tPUSH 1","1\tPUSH $0","2\tADD","3\tNEG","4\tPUSH 4","5\tMUL"],6)
    let got = compileNumExpr compilationContext expr
    fst got `shouldBe` fst expected
    let showCC = show . snd
    showCC got `shouldBe` showCC expected

  it "should parse sample program" $
    case parse parseProgram "" sampleProgram of
      Left err -> fail (show err)
      Right result -> result `shouldBe` solution
      where
        sampleProgram =
          "label loop; var k;\
          \ var j;\
          \ var i;\
          \ read k;\
          \ if k > 0 then begin\
          \ j:=1; i:=k; loop:\
          \ j:=j*i;\
          \ i:=i-1;\
          \ if i > 1 then goto loop;\
          \  print j;\
          \ end;"
        solution =
          ( [ LabelDecl (Ident "loop"),
              VarDecl (Ident "k"),
              VarDecl (Ident "j"),
              VarDecl (Ident "i")
            ],
            [ Input (Ident "k"),
              If
                (Relational (Greater (IdentN "k") (ConstNum 0)))
                ( Block
                    [ Assign (Ident "j") (ConstNum 1),
                      Assign (Ident "i") (IdentN "k"),
                      Tag (Ident "loop") (Assign (Ident "j") (Product (IdentN "j") (IdentN "i"))),
                      Assign (Ident "i") (Subtr (IdentN "i") (ConstNum 1)),
                      If
                        (Relational (Greater (IdentN "i") (ConstNum 1)))
                        (Goto (Ident "loop"))
                        Nothing,
                      Output (IdentN "j")
                    ]
                )
                Nothing
            ])
