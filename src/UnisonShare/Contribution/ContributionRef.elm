module UnisonShare.Contribution.ContributionRef exposing
    ( ContributionRef
    , decode
    , decodeString
    , equals
    , fromInt
    , fromString
    , fromUrl
    , toApiString
    , toString
    , toUrlString
    , unsafeFromString
    )

import Json.Decode as Decode
import Lib.Util as Util
import Parser exposing (Parser)


type ContributionRef
    = ContributionRef Int


fromInt : Int -> Maybe ContributionRef
fromInt n =
    if n > 0 then
        Just (ContributionRef n)

    else
        Nothing


fromString : String -> Maybe ContributionRef
fromString s =
    s
        |> String.toInt
        |> Maybe.andThen fromInt


unsafeFromString : String -> ContributionRef
unsafeFromString s =
    fromString s
        |> Maybe.withDefault (ContributionRef 0)


fromUrl : Parser ContributionRef
fromUrl =
    let
        parseMaybe mversion =
            case mversion of
                Just s_ ->
                    Parser.succeed s_

                Nothing ->
                    Parser.problem "Invalid ContributionRef"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map fromString
        |> Parser.andThen parseMaybe


toString : ContributionRef -> String
toString (ContributionRef n) =
    "#" ++ String.fromInt n


toUrlString : ContributionRef -> String
toUrlString (ContributionRef n) =
    String.fromInt n


toApiString : ContributionRef -> String
toApiString (ContributionRef n) =
    String.fromInt n


equals : ContributionRef -> ContributionRef -> Bool
equals (ContributionRef a) (ContributionRef b) =
    a == b



-- DECODE


decodeString : Decode.Decoder ContributionRef
decodeString =
    Decode.map fromString Decode.string
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ContributionRef")


decode : Decode.Decoder ContributionRef
decode =
    Decode.map fromInt Decode.int
        |> Decode.andThen (Util.decodeFailInvalid "Invalid ContributionRef")
