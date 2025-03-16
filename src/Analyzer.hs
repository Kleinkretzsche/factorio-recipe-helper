module Analyzer where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Prelude hiding (lookup)

import Parser (create_recipe_map, Recipe (..), Ingredient (..))


