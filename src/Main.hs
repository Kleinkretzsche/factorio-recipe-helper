module Main where

import Parser (create_recipe_map)

main :: IO ()
main = do
    m <- create_recipe_map "data/recipes.txt"
    putStrLn $ show m
