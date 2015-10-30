module TW.Support.Lib where

import Json.Decode as JD
import Json.Encode as JE

type AsBase64 = AsBase64 String
jdecAsBase64 : JD.Decoder AsBase64
jdecAsBase64 = JD.map AsBase64 JD.string

jencAsBase64 : AsBase64 -> JE.Value
jencAsBase64 (AsBase64 str) = JE.string str

type Date = Date String
jdecDate : JD.Decoder Date
jdecDate = JD.map Date JD.string

jencDate : Date -> JE.Value
jencDate (Date str) = JE.string str

type Time = Time String
jdecTime : JD.Decoder Time
jdecTime = JD.map Time JD.string

jencTime : Time -> JE.Value
jencTime (Time str) = JE.string str

type DateTime = DateTime String
jdecDateTime : JD.Decoder DateTime
jdecDateTime = JD.map DateTime JD.string

jencDateTime : DateTime -> JE.Value
jencDateTime (DateTime str) = JE.string str

encMaybe : (a -> JE.Value) -> Maybe a -> JE.Value
encMaybe f v =
    case v of
        Nothing -> JE.null
        Just a -> f a
