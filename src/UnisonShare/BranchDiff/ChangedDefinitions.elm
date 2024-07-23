{- A DefinitionDiff collection for easy access, expanded tracking, and memoization -}


module UnisonShare.BranchDiff.ChangedDefinitions exposing (..)

import Code.Syntax exposing (Syntax)
import Dict exposing (Dict)
import RemoteData exposing (WebData)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine)
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
    let
        key =
            ChangeLine.toKey changeLine
    in
    ChangedDefinitions
        (Dict.insert key el changes)


member : ChangedDefinitions -> ChangeLine -> Bool
member (ChangedDefinitions changes) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    Dict.member key changes


isExpanded : ChangedDefinitions -> ChangeLine -> Bool
isExpanded (ChangedDefinitions changes) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    Dict.get key changes
        |> Maybe.map .isExpanded
        |> Maybe.withDefault False


isLoaded : ChangedDefinitions -> ChangeLine -> Bool
isLoaded (ChangedDefinitions changes) changeLine =
    let
        key =
            ChangeLine.toKey changeLine

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


collapse : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
collapse ((ChangedDefinitions changes) as orig) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    Dict.get key changes
        |> Maybe.map (\cd -> { cd | isExpanded = False })
        |> Maybe.map (\cd -> Dict.insert key cd changes)
        |> Maybe.map ChangedDefinitions
        |> Maybe.withDefault orig


expand : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
expand ((ChangedDefinitions changes) as orig) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    Dict.get key changes
        |> Maybe.map (\cd -> { cd | isExpanded = True })
        |> Maybe.map (\cd -> Dict.insert key cd changes)
        |> Maybe.map ChangedDefinitions
        |> Maybe.withDefault orig


toggle : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
toggle changedDefinitions changeLine =
    if isExpanded changedDefinitions changeLine then
        collapse changedDefinitions changeLine

    else
        expand changedDefinitions changeLine


get : ChangedDefinitions -> ChangeLine -> Maybe ChangedDefinition
get (ChangedDefinitions changes) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    Dict.get key changes


remove : ChangedDefinitions -> ChangeLine -> ChangedDefinitions
remove (ChangedDefinitions changes) changeLine =
    let
        key =
            ChangeLine.toKey changeLine
    in
    ChangedDefinitions
        (Dict.remove key changes)
