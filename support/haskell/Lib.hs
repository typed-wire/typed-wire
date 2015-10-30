{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module TW.Support.Lib
    ( AsBase64(..)
    , eatBool
    )
where

import Data.Aeson
import Data.Aeson.Types
import data.Time
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as T

-- | Usual bytestrings, but with base64 aeson repr
newtype AsBase64
    = AsBase64 { unBase64 :: BS.ByteString }
      deriving (Show, Eq, Ord)

instance ToJSON AsBase64 where
    toJSON (AsBase64 bs) =
        String $ T.decodeUtf8 $ B64.encode bs

instance FromJSON AsBase64 where
    parseJSON =
        withText "AsBase64" $ \t ->
        case B64.decode (T.encodeUtf8 t) of
          Left err ->
              fail $ "Can not parse as base64: " ++ err
          Right ok ->
              return $ AsBase64 ok

-- | Enforce bool and ignore it
eatBool :: Bool -> Parser ()
eatBool _ = return ()

-- Extremely hacky until next aeson version includes TimeOfDay
instance ToJSON TimeOfDay where
    toJSON tod =
        case toJSON (LocalTime (ModifiedJulianDay 0) tod) of
          Text x ->
              Text (T.drop 11 x)
          _ -> error "Library error!"

-- Extremely hacky until next aeson version includes TimeOfDay
instance FromJSON TimeOfDay where
    parseJSON =
        withText "TimeOfDay" $ \t ->
        do localTime <- parseJSON (Text $ "1994-02-04 " <> t)
           return $ localTimeOfDay localTime
