module TW.JsonRepr where

import Data.Char

-- | Better version of 'camelTo'. Example where it works better:
--
-- (taken from Aeson library)
--
--   > camelTo '_' 'CamelAPICase' == "camel_apicase"
--   > camelTo2 '_' 'CamelAPICase' == "camel_api_case"
camelTo2 :: Char -> String -> String
camelTo2 c = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : c : u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : c : u : go2 xs
          go2 (x:xs) = x : go2 xs
