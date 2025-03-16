module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Text.Parsec (runParser)
import Parser 
import Logic 

main :: IO ()
main = do
    f <- readFile "data/recipes.txt"
    case (runParser lua_document () "" f) of
        (Right res) -> putStrLn $ show res
        (Left err) -> putStrLn $ show err
