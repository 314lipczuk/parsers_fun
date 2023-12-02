module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec 
symbol :: Parser Char
symbol = oneOf "!#*>?"

someFunc :: IO ()
someFunc = putStrLn "someFucc"