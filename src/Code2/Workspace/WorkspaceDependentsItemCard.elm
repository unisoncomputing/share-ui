module Code2.Workspace.WorkspaceDependentsItemCard exposing (..)

import Code.Definition.Reference exposing (Reference)
import Code2.Workspace.DefinitionItem exposing (DefinitionItem)
import Code2.Workspace.DefinitionMatch exposing (DefinitionMatch)
import Code2.Workspace.DefinitionMatchesState exposing (DefinitionMatchesCardTab, DefinitionMatchesState)
import Code2.Workspace.WorkspaceCard exposing (WorkspaceCard)
import Code2.Workspace.WorkspaceDefinitionMatchesCard as WorkspaceDefinitionMatchesCard
import Code2.Workspace.WorkspaceItemRef exposing (WorkspaceItemRef)
import Lib.String.Helpers exposing (pluralize)


type alias ViewConfig msg =
    { wsRef : WorkspaceItemRef
    , dependentsOfRef : Reference
    , item : DefinitionItem
    , dependents : List DefinitionMatch
    , updateQuery : String -> msg
    , closeItem : msg
    , openDefinition : Reference -> msg
    , state : DefinitionMatchesState
    , changeTab : DefinitionMatchesCardTab -> msg
    }


view : ViewConfig msg -> WorkspaceCard msg
view cfg =
    let
        toSubTitle numDeps =
            String.fromInt numDeps
                ++ " "
                ++ pluralize "direct dependent"
                    "direct dependents"
                    numDeps

        cfg_ =
            { wsRef = cfg.wsRef
            , contextItem = cfg.item
            , contextItemRef = cfg.dependentsOfRef
            , toSubTitle = toSubTitle
            , searchPlaceholder = "Search dependents"
            , emptyStateMessage = "has no direct dependents."
            , matches = cfg.dependents
            , updateQuery = cfg.updateQuery
            , closeItem = cfg.closeItem
            , openDefinition = cfg.openDefinition
            , state = cfg.state
            , changeTab = cfg.changeTab
            }
    in
    WorkspaceDefinitionMatchesCard.view cfg_
