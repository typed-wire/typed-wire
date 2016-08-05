module TW.Types where

import qualified Data.Text as T

data ElmVersion
  = Elm0p16
  | Elm0p17

data LibraryInfo
   = LibraryInfo
   { li_type    :: T.Text
   , li_name    :: T.Text
   , li_version :: T.Text
   }
