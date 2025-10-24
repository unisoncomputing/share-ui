module UnisonShare.ProjectWebhookExample exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Lib.Decode.Helpers as DecodeH
import List.Nonempty as NEL exposing (Nonempty)
import String.Extra as StringE


type alias ProjectWebhookExample =
    { topic : String
    , payload : String
    }


topicFromString : String -> String
topicFromString raw =
    raw
        |> String.replace ":" " "
        |> String.replace "project" ""
        |> StringE.humanize



-- DECODE


decode : Decode.Decoder ProjectWebhookExample
decode =
    let
        jsonToString jsonValue =
            Encode.encode 2 jsonValue
    in
    Decode.map2 ProjectWebhookExample
        (Decode.field "topic" (Decode.map topicFromString Decode.string))
        (Decode.map jsonToString Decode.value)


decodeList : Decode.Decoder (Nonempty ProjectWebhookExample)
decodeList =
    let
        sortByTopic examples =
            NEL.sortBy .topic examples
    in
    Decode.map sortByTopic (DecodeH.nonEmptyList decode)
