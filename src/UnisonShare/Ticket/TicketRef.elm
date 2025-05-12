module UnisonShare.Ticket.TicketRef exposing
    ( TicketRef
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
import Lib.Decode.Helpers exposing (failInvalid)
import Parser exposing (Parser)


type TicketRef
    = TicketRef Int


fromInt : Int -> Maybe TicketRef
fromInt n =
    if n > 0 then
        Just (TicketRef n)

    else
        Nothing


fromString : String -> Maybe TicketRef
fromString s =
    s
        |> String.toInt
        |> Maybe.andThen fromInt


unsafeFromString : String -> TicketRef
unsafeFromString s =
    fromString s
        |> Maybe.withDefault (TicketRef 0)


fromUrl : Parser TicketRef
fromUrl =
    let
        parseMaybe mversion =
            case mversion of
                Just s_ ->
                    Parser.succeed s_

                Nothing ->
                    Parser.problem "Invalid TicketRef"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map fromString
        |> Parser.andThen parseMaybe


toString : TicketRef -> String
toString (TicketRef n) =
    "#" ++ String.fromInt n


toUrlString : TicketRef -> String
toUrlString (TicketRef n) =
    String.fromInt n


toApiString : TicketRef -> String
toApiString (TicketRef n) =
    String.fromInt n


equals : TicketRef -> TicketRef -> Bool
equals (TicketRef a) (TicketRef b) =
    a == b



-- DECODE


decodeString : Decode.Decoder TicketRef
decodeString =
    Decode.map fromString Decode.string
        |> Decode.andThen (failInvalid "Invalid TicketRef")


decode : Decode.Decoder TicketRef
decode =
    Decode.map fromInt Decode.int
        |> Decode.andThen (failInvalid "Invalid TicketRef")
