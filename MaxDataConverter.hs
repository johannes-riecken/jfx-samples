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
    toHaskell cn (SimpleFieldDef tn fn) = addPrefix cn fn <> " :: " <> ucfirst tn

addPrefix :: ClassName -> FieldName -> String
addPrefix cn fn = (fmap toLower . filter isUpper $ cn) <> ucfirst fn

ucfirst :: String -> String
ucfirst [] = []
ucfirst (x:xs) = toUpper x : xs
