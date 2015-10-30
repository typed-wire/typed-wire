module TW.Support.Lib where

import Json.Decode as JD
import Json.Encode as JE

type AsBase64 = AsBase64 String

jdecAsBase64 : JD.Decoder AsBase64
jdecAsBase64 = JD.map AsBase64 JD.string

jencAsBase64 : AsBase64 -> JE.Value
jencAsBase64 (AsBase64 str) = JE.string str

encMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encMaybe f v =
    case v of
        Nothing -> JE.null
        Just a -> f a
