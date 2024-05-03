module UnisonShare.Ticket.TicketRefTests exposing (..)

import Expect
import Test exposing (..)
import UnisonShare.Ticket.TicketRef as TicketRef


fromInt : Test
fromInt =
    describe "TicketRef.fromInt"
        [ test "parse an int into a TicketRef" <|
            \_ ->
                Expect.equal
                    (2023
                        |> TicketRef.fromInt
                        |> Maybe.map TicketRef.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "#2023"
        , test "fails to parse int of 0" <|
            \_ ->
                Expect.equal
                    (TicketRef.fromInt 0)
                    Nothing
        , test "fails to parse a negative int" <|
            \_ ->
                Expect.equal
                    (TicketRef.fromInt -9)
                    Nothing
        ]


fromString : Test
fromString =
    describe "TicketRef.fromString"
        [ test "parse a string into a TicketRef" <|
            \_ ->
                Expect.equal
                    ("2023"
                        |> TicketRef.fromString
                        |> Maybe.map TicketRef.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "#2023"
        , test "fails to parse an invalid string" <|
            \_ ->
                Expect.equal
                    (TicketRef.fromString "invalid")
                    Nothing
        ]


toString : Test
toString =
    describe "TicketRef.toString"
        [ test "render the ref as a string" <|
            \_ ->
                Expect.equal "#2023" (TicketRef.toString (TicketRef.unsafeFromString "2023"))
        ]
