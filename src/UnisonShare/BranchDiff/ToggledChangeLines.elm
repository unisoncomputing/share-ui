module UnisonShare.BranchDiff.ToggledChangeLines exposing (..)

import Dict exposing (Dict)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId


type ToggledChangeLine
    = Expanded
    | Collapsed


type ToggledChangeLines
    = ToggledChangeLines (Dict String ToggledChangeLine)



-- CREATE


empty : ToggledChangeLines
empty =
    ToggledChangeLines Dict.empty


isCollapsed : ToggledChangeLines -> ChangeLine -> Bool
isCollapsed (ToggledChangeLines changes) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId

                withDefaultCollapsed =
                    ChangeLine.shouldBeCollapsedByDefault changeLine
            in
            Dict.get key changes
                |> Maybe.map ((==) Collapsed)
                |> Maybe.withDefault withDefaultCollapsed

        Nothing ->
            False


isExpanded : ToggledChangeLines -> ChangeLine -> Bool
isExpanded (ToggledChangeLines changes) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId

                withDefaultExpanded =
                    not (ChangeLine.shouldBeCollapsedByDefault changeLine)
            in
            Dict.get key changes
                |> Maybe.map ((==) Expanded)
                |> Maybe.withDefault withDefaultExpanded

        Nothing ->
            False


collapse : ToggledChangeLines -> ChangeLine -> ToggledChangeLines
collapse ((ToggledChangeLines changes) as orig) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            ToggledChangeLines (Dict.insert key Collapsed changes)

        Nothing ->
            orig


expand : ToggledChangeLines -> ChangeLine -> ToggledChangeLines
expand ((ToggledChangeLines changes) as orig) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            ToggledChangeLines (Dict.insert key Expanded changes)

        Nothing ->
            orig
