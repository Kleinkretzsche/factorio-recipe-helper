module Parser where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (lookup)

import Text.Parsec
import Text.Parsec.String (Parser)

data Lua_Value 
  =  Lua_String String 
  |  Lua_Bool Bool
  |  Lua_Int Int 
  |  Lua_Float Float 
  |  Lua_Array [Lua_Value] 
  |  Lua_Object [(String, Lua_Value)] 
  deriving(Eq, Show)

type Recipe_Map = Map String Recipe

data Part = { kind :: String, amount :: Float }deriving(Show)
type Ingredient = (String, Part)
data Ingredients = Map String (String, Float)

data Recipe = Recipe 
  { name :: String
  , ingredients :: Ingredients
  , results :: Ingredients
  } deriving (Show, Eq)

whitespace :: Parser ()
whitespace = do
  _ <- many $ (try comment_parser <|> try (void $ satisfy (`elem` " \n\t\r")))
  return ()

void :: Parser a -> Parser ()
void p = do
    _ <- p 
    return ()

comment_parser :: Parser ()
comment_parser = 
  string "--" 
  >> (many $ satisfy (`elem` " \t")) 
  >> (many $ satisfy (/= '\n')) 
  >> (void $ char '\n')

array_of :: Parser a -> Parser [a]
array_of p = do
    ps <- between (char '{' <* whitespace) (whitespace *> char '}') 
       $ (p <* whitespace) `sepEndBy` (char ',' <* whitespace)
    return ps

lua_str :: Parser Lua_Value
lua_str = do
    str <- between (char '"') (char '"') (many $ satisfy (/= '"'))
    return $ Lua_String str

lua_num :: Parser Lua_Value
lua_num = do
    f <- many1 (digit <|> char '.')
    case ('.' `elem` f) of
        True  -> return $ Lua_Float (read f)
        False -> return $ Lua_Int (read f)

lua_bool :: Parser Lua_Value
lua_bool = do
    str <- (string "false") <|> (string "true")
    case str of 
        "true" -> return $ Lua_Bool True
        _ -> return $ Lua_Bool False

lua_object :: Parser Lua_Value
lua_object = do
    void $ lookAhead $ (char '{' >> whitespace >> (parsecMap snd kv_parser))
    kvs <- array_of kv_parser
    return $ Lua_Object kvs

lua_array :: Parser Lua_Value
lua_array = do
    vals <- array_of lua_parser
    return $ Lua_Array vals

lua_parser :: Parser Lua_Value
lua_parser = try lua_object
  <|> try lua_array
  <|> try lua_str
  <|> try lua_num 
  <|> try lua_bool

kv_parser :: Parser (String, Lua_Value) 
kv_parser = do
    key <- many1 $ satisfy (`notElem` "#- ={\t\n\"};)")
    void $ (whitespace *> char '=' <* whitespace)
    val <- lua_parser
    return (key, val)

lua_document :: Parser Lua_Value
lua_document = 
    string "data:extend" *> whitespace *> char '(' *> whitespace *> 
      lua_parser 
    <* char ')' <* whitespace <* eof

recipe_map :: Lua_Value -> Recipe_Map
recipe_map (Lua_Array rs) = 
  case (sequenceA $ map recipe_from_obj rs) of
    (Just r) -> Map.fromList $ map (\rec@(Recipe n _ _) -> (n, rec)) r
    _ -> Map.empty
recipe_map _ = Map.empty

create_recipe_map :: String -> IO Recipe_Map
create_recipe_map file_path = do
    f <- readFile file_path
    case (runParser lua_document () "" f) of
        (Right res) -> return $ recipe_map res
        (Left _) -> return Map.empty

obj_to_map :: Lua_Value -> Map String Lua_Value
obj_to_map (Lua_Object o) = Map.fromList o
obj_to_map _ = Map.empty

ingred_from_obj :: Lua_Value -> Maybe Ingredient
ingred_from_obj o@(Lua_Object _) = 
  case (sequenceA $ map (\x -> Map.lookup x $ obj_to_map o) fields) of 
    Just [(Lua_String t), (Lua_String n), (Lua_Int a)] 
      -> Just $ (n, (t, a))
    _ -> Nothing
  where 
    fields = ["type", "name", "amount"]
ingred_from_obj _ = Nothing

recipe_from_parts :: Maybe [Lua_Value] -> Maybe Recipe
recipe_from_parts (Just [(Lua_String n), (Lua_Array is), (Lua_Array rs)]) = do
  ingredients <- sequenceA $ map ingred_from_obj is
  results     <- sequenceA $ map ingred_from_obj rs
  return $ Recipe n ingredients results
recipe_from_parts _ = Nothing

recipe_from_obj :: Lua_Value -> Maybe Recipe
recipe_from_obj o@(Lua_Object _) = recipe_from_parts ps
  where 
    ps = (sequenceA $ map (\x -> Map.lookup x $ obj_to_map o) fields)
    fields = ["name", "ingredients", "results"]
recipe_from_obj _ = Nothing
