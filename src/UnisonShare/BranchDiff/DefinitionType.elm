module UnisonShare.BranchDiff.DefinitionType exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.HashQualified exposing (HashQualified)


type DefinitionType
    = Term
    | Type
    | Ability
    | Doc
    | Test
    | DataConstructor
    | AbilityConstructor


isTerm : DefinitionType -> Bool
isTerm dt =
    case dt of
        Term ->
            True

        Doc ->
            True

        Test ->
            True

        _ ->
            False


isType : DefinitionType -> Bool
isType dt =
    not (isTerm dt)


toReferenceConstructor : DefinitionType -> (HashQualified -> Reference)
toReferenceConstructor dt =
    case dt of
        Term ->
            Reference.TermReference

        Type ->
            Reference.TypeReference

        Ability ->
            Reference.TypeReference

        Doc ->
            Reference.TermReference

        Test ->
            Reference.TermReference

        AbilityConstructor ->
            Reference.AbilityConstructorReference

        DataConstructor ->
            Reference.DataConstructorReference


equals : DefinitionType -> DefinitionType -> Bool
equals a b =
    a == b


toString : DefinitionType -> String
toString dt =
    case dt of
        Term ->
            "term"

        Type ->
            "type"

        Doc ->
            "doc"

        Ability ->
            "ability"

        AbilityConstructor ->
            "ability-constructor"

        DataConstructor ->
            "data-constructor"

        Test ->
            "test"


fromString : String -> Maybe DefinitionType
fromString dt =
    case dt of
        "term" ->
            Just Term

        "type" ->
            Just Type

        "doc" ->
            Just Doc

        "ability" ->
            Just Ability

        "ability-constructor" ->
            Just AbilityConstructor

        "data-constructor" ->
            Just DataConstructor

        "test" ->
            Just Test

        _ ->
            Nothing
