module TW.Types where

import qualified Data.Text as T

data ElmVersion
  = Elm0p16
  | Elm0p17 deriving (Enum)

instance Show ElmVersion where
  show Elm0p17 = "0.17"
  show Elm0p16 = "0.16"

data LibraryInfo
   = LibraryInfo
   { li_type    :: T.Text
   , li_name    :: T.Text
   , li_version :: T.Text
   }
