module UnisonShare.Tour exposing (..)

import Json.Decode as Decode


type Tour
    = WelcomeTerms



-- HELPERS


toString : Tour -> String
toString _ =
    "welcome-terms"



-- DECODE


decode : Decode.Decoder Tour
decode =
    Decode.succeed WelcomeTerms
