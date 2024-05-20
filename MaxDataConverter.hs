{-# OPTIONS_GHC -Wall #-}
module MaxDataConverter where
import Data.Char
import Data.List (intercalate)

import DataTypes
import MaxDataParser

translate :: String -> Either String String
translate src = do
    cd <- parseClassDef src
    pure $ toHaskell "" cd

class ToHaskell a where
    -- needs class name for field prefix
    toHaskell :: ClassName -> a -> String

instance ToHaskell ClassDef where
    toHaskell _ (SimpleClassDef cn fds) = "data " <> cn <> " = " <> cn <> " { " <> intercalate ", " (fmap (toHaskell cn) fds) <> " }"

instance ToHaskell FieldDef where
    toHaskell cn (SimpleFieldDef tn fn) = addPrefix cn fn <> " :: " <> typeNameToHaskell tn
    toHaskell cn (ArrayFieldDef tn fn) = addPrefix cn fn <> " :: " <> "[" <> typeNameToHaskell tn <> "]"
    toHaskell cn (MapFieldDef k v fn) = addPrefix cn fn <> " :: " <> "Map " <> typeNameToHaskell k <> " " <> typeNameToHaskell v
    toHaskell cn (ListFieldDef tn fn) = addPrefix cn fn <> " :: " <> "[" <> typeNameToHaskell tn <> "]"

typeNameToHaskell :: TypeName -> String
typeNameToHaskell "Integer" = "Int"
typeNameToHaskell xs = ucfirst xs


addPrefix :: ClassName -> FieldName -> String
addPrefix cn fn = let guess = filter isUpper cn
    in fmap toLower (if length guess > 1
    then guess
    else fmap toLower . take 3 $ cn) <> ucfirst fn
ucfirst :: String -> String
ucfirst [] = []
ucfirst (x:xs) = toUpper x : xs
