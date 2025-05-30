module UnisonShare.UnisonPlan exposing (UnisonPlan(..), decode, fromString, toString)

import Json.Decode as Decode


type UnisonPlan
    = Free
    | Starter
    | Pro


fromString : String -> UnisonPlan
fromString s =
    case s of
        "Free" ->
            Free

        "Starter" ->
            Starter

        "Pro" ->
            Pro

        _ ->
            Free


toString : UnisonPlan -> String
toString p =
    case p of
        Free ->
            "Free"

        Starter ->
            "Starter"

        Pro ->
            "Pro"


decode : Decode.Decoder UnisonPlan
decode =
    Decode.map fromString Decode.string
