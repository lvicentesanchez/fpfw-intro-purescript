module Model.User
  ( User(..)
  , fromRegistration
  ) where

import Prelude

import Data.Array (singleton)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.Validation (V, invalid)

newtype User = User { name :: String, surname :: String, age :: Int }

nonEmpty :: String -> String -> V (Array String) String
nonEmpty prop "" = invalid (singleton ("'" ++ prop ++ "' is empty."))
nonEmpty _ value = pure value

isInteger :: String -> String -> V (Array String) Int
isInteger prop num = maybe errorMsg pure (fromString num)
  where
  errorMsg = invalid (singleton ("'" ++ prop ++ "' is not a number."))

fromRegistration :: String -> String -> String -> V (Array String) User
fromRegistration name surname age =
  fromValid
  <$> nonEmpty "name" name
  <*> nonEmpty "surname" surname
  <*> isInteger "age" age
  where
    fromValid name surname age = User { name, surname, age}
