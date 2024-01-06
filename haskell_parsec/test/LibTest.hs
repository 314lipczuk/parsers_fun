{-# LANGUAGE OverloadedStrings #-}
module Main where
import Text.Megaparsec (parse , eof)
import Lib 
import Test.Tasty.Hspec
import Test.Hspec
import Data.List(isPrefixOf)

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
    
