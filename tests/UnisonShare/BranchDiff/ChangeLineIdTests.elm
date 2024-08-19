module UnisonShare.BranchDiff.ChangeLineIdTests exposing (..)

import Code.FullyQualifiedName as FQN
import Expect
import Test exposing (..)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId
import UnisonShare.BranchDiff.DefinitionType as DefinitionType


equals : Test
equals =
    describe "ChangeLineId.equals"
        [ test "True when the ids are the same" <|
            \_ ->
                let
                    a =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")

                    b =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")
                in
                Expect.equal
                    True
                    (ChangeLineId.equals a b)
        , test "False when the ids are not the same" <|
            \_ ->
                let
                    a =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")

                    b =
                        ChangeLineId.changeLineId ChangeLineId.Removed DefinitionType.Term (FQN.fromString "List.map")
                in
                Expect.equal
                    False
                    (ChangeLineId.equals a b)
        ]


fromString : Test
fromString =
    describe "ChangeLineId.fromString"
        [ test "parse a string into a ChangeLineId" <|
            \_ ->
                Expect.equal
                    ("added-term-List.map"
                        |> ChangeLineId.fromString
                        |> Maybe.map ChangeLineId.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "added-term-List.map"
        , test "parse a string with an _ in the definition name into a ChangeLineId" <|
            \_ ->
                Expect.equal
                    ("added-term-List.map_"
                        |> ChangeLineId.fromString
                        |> Maybe.map ChangeLineId.toString
                        |> Maybe.withDefault "FAIL!"
                    )
                    "added-term-List.map_"
        , test "fails to parse an invalid string" <|
            \_ ->
                Expect.equal
                    (ChangeLineId.fromString "invalid")
                    Nothing
        ]


toString : Test
toString =
    describe "ChangeLineId.toString"
        [ test "render the id as a string" <|
            \_ ->
                let
                    id =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")
                in
                Expect.equal "added-term-List.map" (ChangeLineId.toString id)
        ]


toKey : Test
toKey =
    describe "ChangeLineId.toKey"
        [ test "render the id as a key" <|
            \_ ->
                let
                    id =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")
                in
                Expect.equal "added-term-List.map" (ChangeLineId.toKey id)
        ]


toDomId : Test
toDomId =
    describe "ChangeLineId.toDomId"
        [ test "render the id as a domId" <|
            \_ ->
                let
                    id =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term (FQN.fromString "List.map")
                in
                Expect.equal "added-term-List__map" (ChangeLineId.toDomId id)
        ]
