module TW.Support.Lib where

import Date as D
import String as S
import Json.Decode as JD
import Json.Encode as JE

type AsBase64 = AsBase64 String
jdecAsBase64 : JD.Decoder AsBase64
jdecAsBase64 = JD.map AsBase64 JD.string

jencAsBase64 : AsBase64 -> JE.Value
jencAsBase64 (AsBase64 str) = JE.string str

type Date = Date String

toStdDate : Date -> Maybe D.Date
toStdDate (Date dateStr) =
    case D.fromString dateStr of
        Err _ -> Nothing
        Ok val -> Just val

fromStdDate : D.Date -> Date
fromStdDate d =
    let y = D.year d |> toString
        m = D.month d |> monthToInt |> toString |> S.padLeft 2 '0'
        day = D.day d |> toString |> S.padLeft 2 '0'
    in Date <| y ++ "-" ++ m ++ "-" ++ day

monthToInt : D.Month -> Int
monthToInt m =
    case m of
        D.Jan -> 1
        D.Feb -> 2
        D.Mar -> 3
        D.Apr -> 4
        D.May -> 5
        D.Jun -> 6
        D.Jul -> 7
        D.Aug -> 8
        D.Sep -> 9
        D.Oct -> 10
        D.Nov -> 11
        D.Dec -> 12

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
