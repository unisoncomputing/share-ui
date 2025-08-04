module UnisonShare.ProjectWebhook exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers as DecodeH
import Url exposing (Url)


type alias ProjectWebhook =
    { url : Url
    , events : List String
    }


decode : Decode.Decoder ProjectWebhook
decode =
    Decode.succeed ProjectWebhook
        |> required "url" DecodeH.url
        |> required "events" (Decode.list Decode.string)
