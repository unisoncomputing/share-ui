{- A DefinitionDiff collection for easy access, expanded tracking, and memoization -}


module UnisonShare.BranchDiff.ChangedDefinitions exposing (..)

import Code.Syntax exposing (Syntax)
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId
import UnisonShare.DefinitionDiff exposing (DefinitionDiff)


type ChangedDefinitionDetail
    = Diff (WebData DefinitionDiff)
    | DefinitionSyntax (WebData Syntax)


type alias ChangedDefinition =
    { isExpanded : Bool, data : ChangedDefinitionDetail }


type ChangedDefinitions
    = ChangedDefinitions (Dict String ChangedDefinition)



-- CREATE


empty : ChangedDefinitions
empty =
    ChangedDefinitions Dict.empty


set : ChangedDefinitions -> ChangeLine -> ChangedDefinition -> ChangedDefinitions
set (ChangedDefinitions changes) changeLine el =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            ChangedDefinitions
                (Dict.insert key el changes)

        Nothing ->
            ChangedDefinitions changes


member : ChangedDefinitions -> ChangeLine -> Bool
member (ChangedDefinitions changes) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            Dict.member key changes

        Nothing ->
            False


isExpanded : ChangedDefinitions -> ChangeLine -> Bool
isExpanded (ChangedDefinitions changes) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            Dict.get key changes
                |> Maybe.map .isExpanded
                |> Maybe.withDefault False

        Nothing ->
            False


isLoaded : ChangedDefinitions -> ChangeLine -> Bool
isLoaded (ChangedDefinitions changes) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId

                isLoaded_ detail =
                    case detail of
                        Diff d ->
                            RemoteData.isSuccess d

                        DefinitionSyntax d ->
                            RemoteData.isSuccess d
            in
            Dict.get key changes
                |> Maybe.map (.data >> isLoaded_)
                |> Maybe.withDefault False

        Nothing ->
            False


collapse : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
collapse ((ChangedDefinitions changes) as orig) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            Dict.get key changes
                |> Maybe.map (\cd -> { cd | isExpanded = False })
                |> Maybe.map (\cd -> Dict.insert key cd changes)
                |> Maybe.map ChangedDefinitions
                |> Maybe.withDefault orig

        Nothing ->
            orig


expand : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
expand ((ChangedDefinitions changes) as orig) changeLine =
    case ChangeLine.toChangeLineId changeLine of
        Just changeLineId ->
            let
                key =
                    ChangeLineId.toKey changeLineId
            in
            Dict.get key changes
                |> Maybe.map (\cd -> { cd | isExpanded = True })
                |> Maybe.map (\cd -> Dict.insert key cd changes)
                |> Maybe.map ChangedDefinitions
                |> Maybe.withDefault orig

        Nothing ->
            orig


toggle : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
toggle changedDefinitions changeLine =
    if isExpanded changedDefinitions changeLine then
        collapse changedDefinitions changeLine

    else
        expand changedDefinitions changeLine


get : ChangedDefinitions -> ChangeLine -> Maybe ChangedDefinition
get (ChangedDefinitions changes) changeLine =
    changeLine
        |> ChangeLine.toChangeLineId
        |> Maybe.map ChangeLineId.toKey
        |> Maybe.andThen (\key -> Dict.get key changes)


remove : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
remove ((ChangedDefinitions changes) as orig) changeLine =
    changeLine
        |> ChangeLine.toChangeLineId
        |> Maybe.map ChangeLineId.toKey
        |> Maybe.map (\key -> ChangedDefinitions (Dict.remove key changes))
        |> Maybe.withDefault orig
