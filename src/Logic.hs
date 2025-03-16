module Logic (recipe_map) where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (lookup)

import Parser (Lua_Value (..))

data Ingredient = Ingredient
  { kind :: String
  , name :: String
  , amount :: Int
  } deriving(Show, Eq)

data Recipe = Recipe 
  { _name :: String
  , _ingredients :: [Ingredient]
  , _results :: [Ingredient]
  } deriving(Show, Eq)

recipe_map :: Lua_Value -> Map String Recipe
recipe_map (Lua_Array rs) = 
  case (sequenceA $ map recipe_from_obj rs) of
    (Just rs) -> Map.fromList $ map (\r@(Recipe n _ _) -> (n, r)) rs
    _ -> Map.empty
recipe_map _ = Map.empty

obj_to_map :: Lua_Value -> Map String Lua_Value
obj_to_map (Lua_Object o) = Map.fromList o
obj_to_map _ = Map.empty

ingred_from_obj :: Lua_Value -> Maybe Ingredient
ingred_from_obj o@(Lua_Object _) = 
  case (sequenceA $ map (\x -> Map.lookup x $ obj_to_map o) fields) of 
    Just [(Lua_String t), (Lua_String n), (Lua_Int a)] 
      -> Just $ Ingredient t n a
    _ -> Nothing
  where 
    fields = ["type", "name", "amount"]
_ = Nothing

recipe_from_parts :: Maybe [Lua_Value] -> Maybe Recipe
recipe_from_parts (Just [(Lua_String n), (Lua_Array is), (Lua_Array rs)]) = do
  let name = n
  ingredients <- sequenceA $ map ingred_from_obj is
  results     <- sequenceA $ map ingred_from_obj rs
  return $ Recipe name ingredients results
recipe_from_parts _ = Nothing

recipe_from_obj :: Lua_Value -> Maybe Recipe
recipe_from_obj o@(Lua_Object _) = recipe_from_parts ps
  where 
    ps = (sequenceA $ map (\x -> Map.lookup x $ obj_to_map o) fields)
    fields = ["name", "ingredients", "results"]
_ = Nothing
