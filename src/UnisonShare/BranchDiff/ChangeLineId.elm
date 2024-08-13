module UnisonShare.BranchDiff.ChangeLineId exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Parser exposing (Parser)
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType)


type ChangeLineType
    = Added
    | Removed
    | Updated
    | RenamedFrom
    | Aliased


type ChangeLineId
    = ChangeLineId
        { changeType : ChangeLineType
        , definitionType : DefinitionType
        , fqn : FQN
        }


changeLineId : ChangeLineType -> DefinitionType -> FQN -> ChangeLineId
changeLineId ct dt fqn =
    ChangeLineId
        { definitionType = dt
        , changeType = ct
        , fqn = fqn
        }


toKey : ChangeLineId -> String
toKey cl =
    toString cl


toDomId : ChangeLineId -> String
toDomId (ChangeLineId { definitionType, changeType, fqn }) =
    let
        type_ =
            DefinitionType.toString definitionType

        -- DOM ids do not support dots
        fqnToKeyPart =
            fqn |> FQN.toString |> String.replace "." "__"

        key_ =
            [ changeLineTypeToString changeType, type_, fqnToKeyPart ]
    in
    String.join "_" key_


toString : ChangeLineId -> String
toString (ChangeLineId { definitionType, changeType, fqn }) =
    let
        type_ =
            DefinitionType.toString definitionType

        key_ =
            [ changeLineTypeToString changeType, type_, FQN.toString fqn ]
    in
    String.join "_" key_


fromString : String -> Maybe ChangeLineId
fromString raw =
    case String.split "_" raw of
        [ rawChangeLineType, rawDefinitionType, rawFQN ] ->
            Maybe.map3
                changeLineId
                (changeLineTypeFromString rawChangeLineType)
                (DefinitionType.fromString rawDefinitionType)
                (Just (FQN.fromString rawFQN))

        _ ->
            Nothing


fromUrl : Parser ChangeLineId
fromUrl =
    let
        parseMaybe clid =
            case clid of
                Just clid_ ->
                    Parser.succeed clid_

                Nothing ->
                    Parser.problem "Invalid ChangeLineId"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map fromString
        |> Parser.andThen parseMaybe



-- INTERNAL


changeLineTypeToString : ChangeLineType -> String
changeLineTypeToString changeLineType =
    case changeLineType of
        Added ->
            "added"

        Removed ->
            "removed"

        Updated ->
            "updated"

        RenamedFrom ->
            "renamed"

        Aliased ->
            "aliased"


changeLineTypeFromString : String -> Maybe ChangeLineType
changeLineTypeFromString raw =
    case raw of
        "added" ->
            Just Added

        "removed" ->
            Just Removed

        "updated" ->
            Just Updated

        "renamed" ->
            Just RenamedFrom

        "aliased" ->
            Just Aliased

        _ ->
            Nothing
