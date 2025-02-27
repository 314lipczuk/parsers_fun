{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Lib (compile, parseProgram)
import Data.Text (pack)
import Data.Text
import Data.Text.IO

import Text.Megaparsec
import Text.Megaparsec.Char
main :: IO ()
main = do
  input <- Prelude.getContents
  let parsed = parse parseProgram "" (pack input)
  case parsed of
    Left err -> fail (show err)
    Right o ->  do
      let (t,ct) = compile o
      --printContext ct
      Data.Text.IO.putStrLn t
