module UnisonShare.Contribution.ContributionRefTests exposing (..)

import Expect
import Test exposing (..)
import UnisonShare.Contribution.ContributionRef as ContributionRef


fromInt : Test
fromInt =
    describe "ContributionRef.fromInt"
        [ test "parse an int into a ContributionRef" <|
            \_ ->
                Expect.equal
                    (2023
                        |> ContributionRef.fromInt
                        |> Maybe.map ContributionRef.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "#2023"
        , test "fails to parse int of 0" <|
            \_ ->
                Expect.equal
                    (ContributionRef.fromInt 0)
                    Nothing
        , test "fails to parse a negative int" <|
            \_ ->
                Expect.equal
                    (ContributionRef.fromInt -9)
                    Nothing
        ]


fromString : Test
fromString =
    describe "ContributionRef.fromString"
        [ test "parse a string into a ContributionRef" <|
            \_ ->
                Expect.equal
                    ("2023"
                        |> ContributionRef.fromString
                        |> Maybe.map ContributionRef.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "#2023"
        , test "fails to parse an invalid string" <|
            \_ ->
                Expect.equal
                    (ContributionRef.fromString "invalid")
                    Nothing
        ]


toString : Test
toString =
    describe "ContributionRef.toString"
        [ test "render the ref as a string" <|
            \_ ->
                Expect.equal "#2023" (ContributionRef.toString (ContributionRef.unsafeFromString "2023"))
        ]
