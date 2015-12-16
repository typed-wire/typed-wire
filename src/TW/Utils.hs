module TW.Utils where

import Data.Char
import qualified Data.Text as T

capitalizeText :: T.Text -> T.Text
capitalizeText =
    T.pack . go . T.unpack
    where
       go (x:xs) = toUpper x : xs
       go [] = []
