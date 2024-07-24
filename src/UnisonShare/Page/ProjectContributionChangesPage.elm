module UnisonShare.Page.ProjectContributionChangesPage exposing (..)

import Code.BranchRef exposing (BranchRef)
import Code.Definition.Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Perspective as Perspective
import Code.Syntax as Syntax
import Code.Syntax.Linked as SyntaxLinked
import Html exposing (Html, code, div, pre, span, strong, text)
import Html.Attributes exposing (class, id)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (pluralize)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchDiff as BranchDiff exposing (BranchDiff)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine)
import UnisonShare.BranchDiff.ChangedDefinitions as ChangedDefinitions exposing (ChangedDefinitions)
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType)
import UnisonShare.Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)



-- MODEL


type alias Model =
    { branchDiff : WebData BranchDiff
    , changedDefinitions : ChangedDefinitions
    }


type alias DiffBranches =
    { oldBranch : BranchDiff.DiffBranchRef
    , newBranch : BranchDiff.DiffBranchRef
    }


init : AppContext -> ProjectRef -> ContributionRef -> Maybe FQN.FQN -> ( Model, Cmd Msg )
init appContext projectRef contribRef _ =
    ( { branchDiff = Loading
      , changedDefinitions = ChangedDefinitions.empty
      }
    , fetchBranchDiff appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = FetchBranchDiffFinished (WebData BranchDiff)
    | ExpandChangeDetails ChangeLine
    | ToggleChangeDetails ChangeLine
    | FetchDefinitionDiffFinished ChangeLine (WebData DefinitionDiff)
    | FetchDefinitionSyntaxFinished ChangeLine (WebData Syntax.Syntax)
    | NoOp


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef _ msg model =
    case msg of
        FetchBranchDiffFinished branchDiff ->
            let
                branchDiff_ =
                    RemoteData.map
                        (\bd ->
                            { bd
                                | lines = BranchDiff.condense bd.lines
                            }
                        )
                        branchDiff
            in
            ( { model | branchDiff = branchDiff_ }, Cmd.none )

        ExpandChangeDetails changeLine ->
            case model.branchDiff of
                Success branchDiff ->
                    expandAndScrollTo appContext projectRef model branchDiff changeLine

                _ ->
                    ( model, Cmd.none )

        ToggleChangeDetails changeLine ->
            case model.branchDiff of
                Success branchDiff ->
                    if ChangedDefinitions.isExpanded model.changedDefinitions changeLine then
                        ( { model
                            | changedDefinitions =
                                ChangedDefinitions.collapse model.changedDefinitions changeLine
                          }
                        , Cmd.none
                        )

                    else
                        expandAndScrollTo appContext projectRef model branchDiff changeLine

                _ ->
                    ( model, Cmd.none )

        FetchDefinitionDiffFinished changeLine resp ->
            let
                isExpanded =
                    ChangedDefinitions.isExpanded model.changedDefinitions changeLine

                changedDefinitions =
                    ChangedDefinitions.set model.changedDefinitions
                        changeLine
                        { isExpanded = isExpanded
                        , data =
                            resp
                                |> RemoteData.map DefinitionDiff.addReplaceSegments
                                |> ChangedDefinitions.Diff
                        }
            in
            ( { model | changedDefinitions = changedDefinitions }, Cmd.none )

        FetchDefinitionSyntaxFinished changeLine resp ->
            let
                isExpanded =
                    ChangedDefinitions.isExpanded model.changedDefinitions changeLine

                changedDefinitions =
                    ChangedDefinitions.set model.changedDefinitions
                        changeLine
                        { isExpanded = isExpanded
                        , data = ChangedDefinitions.DefinitionSyntax resp
                        }
            in
            ( { model | changedDefinitions = changedDefinitions }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- UPDATE HELPERS


expandAndScrollTo : AppContext -> ProjectRef -> Model -> BranchDiff -> ChangeLine -> ( Model, Cmd Msg )
expandAndScrollTo appContext projectRef model branchDiff changeLine =
    if ChangedDefinitions.isLoaded model.changedDefinitions changeLine then
        ( { model
            | changedDefinitions =
                ChangedDefinitions.expand model.changedDefinitions changeLine
          }
        , ScrollTo.scrollTo NoOp "page-content" (ChangeLine.toKey changeLine)
        )

    else
        let
            changedDefinitions =
                ChangedDefinitions.set model.changedDefinitions
                    changeLine
                    { isExpanded = True, data = ChangedDefinitions.Diff Loading }

            fetchTermSyntax_ branchRef name =
                fetchTermSyntax appContext projectRef branchRef changeLine name

            fetchTypeSyntax_ branchRef name =
                fetchTypeSyntax appContext projectRef branchRef changeLine name

            fetchTermDefinitionDiff name =
                fetchDefinitionDiff appContext
                    projectRef
                    DefinitionDiff.Term
                    changeLine
                    (ShareApi.Term
                        { oldBranchRef = branchDiff.oldBranch.ref
                        , newBranchRef = branchDiff.newBranch.ref
                        , oldTerm = name
                        , newTerm = name
                        }
                    )

            fetchTypeDefinitionDiff name =
                fetchDefinitionDiff appContext
                    projectRef
                    DefinitionDiff.Type
                    changeLine
                    (ShareApi.Type
                        { oldBranchRef = branchDiff.oldBranch.ref
                        , newBranchRef = branchDiff.newBranch.ref
                        , oldType = name
                        , newType = name
                        }
                    )

            fetchSyntax_ branchRef =
                case ChangeLine.definitionType changeLine of
                    Just dt ->
                        if DefinitionType.isTerm dt then
                            fetchTermSyntax_ branchRef (ChangeLine.fullName changeLine)

                        else
                            fetchTypeSyntax_ branchRef (ChangeLine.fullName changeLine)

                    Nothing ->
                        Cmd.none

            cmd =
                case changeLine of
                    ChangeLine.Updated dt { fullName } ->
                        if DefinitionType.isTerm dt then
                            fetchTermDefinitionDiff fullName

                        else
                            fetchTypeDefinitionDiff fullName

                    ChangeLine.Removed _ _ ->
                        fetchSyntax_ branchDiff.oldBranch.ref

                    _ ->
                        fetchSyntax_ branchDiff.newBranch.ref
        in
        ( { model | changedDefinitions = changedDefinitions }
        , Cmd.batch
            [ cmd
            , ScrollTo.scrollTo NoOp "page-content" (ChangeLine.toKey changeLine)
            ]
        )



-- EFFECTS


fetchBranchDiff : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchBranchDiff appContext projectRef contributionRef =
    ShareApi.projectContributionDiff projectRef contributionRef
        |> HttpApi.toRequest BranchDiff.decode (RemoteData.fromResult >> FetchBranchDiffFinished)
        |> HttpApi.perform appContext.api


fetchDefinitionDiff :
    AppContext
    -> ProjectRef
    -> DefinitionDiff.DefinitionType
    -> ChangeLine
    -> ShareApi.DefinitionDiffParams
    -> Cmd Msg
fetchDefinitionDiff appContext projectRef definitionType changeLine params =
    ShareApi.projectBranchDefinitionDiff projectRef params
        |> HttpApi.toRequest
            (DefinitionDiff.decode definitionType)
            (RemoteData.fromResult >> FetchDefinitionDiffFinished changeLine)
        |> HttpApi.perform appContext.api


fetchTermSyntax : AppContext -> ProjectRef -> BranchRef -> ChangeLine -> FQN.FQN -> Cmd Msg
fetchTermSyntax appContext projectRef branchRef changeLine name =
    fetchSyntax appContext projectRef branchRef changeLine "term" name


fetchTypeSyntax : AppContext -> ProjectRef -> BranchRef -> ChangeLine -> FQN.FQN -> Cmd Msg
fetchTypeSyntax appContext projectRef branchRef changeLine name =
    fetchSyntax appContext projectRef branchRef changeLine "type" name


fetchSyntax : AppContext -> ProjectRef -> BranchRef -> ChangeLine -> String -> FQN.FQN -> Cmd Msg
fetchSyntax appContext projectRef branchRef changeLine fieldPrefix name =
    let
        decode_ : Decode.Decoder Syntax.Syntax
        decode_ =
            Decode.field (fieldPrefix ++ "Definitions")
                (Decode.keyValuePairs (Decode.at [ fieldPrefix ++ "Definition", "contents" ] Syntax.decode)
                    -- The result (after Syntax.decode above) is a (List (Hash,
                    -- Syntax)). We just want the syntax part and only the first
                    -- one (disregarding naming collisions)
                    |> Decode.map (List.map Tuple.second >> List.head)
                    |> Decode.andThen (Util.decodeFailInvalid "No valid definitions returned")
                )
    in
    ShareApi.projectBranchDefinitionByName projectRef branchRef name
        |> HttpApi.toRequest decode_
            (RemoteData.fromResult >> FetchDefinitionSyntaxFinished changeLine)
        |> HttpApi.perform appContext.api



-- VIEW


branchLink_ : ProjectRef -> BranchDiff.DiffBranchRef -> Reference -> Click.Click Msg
branchLink_ projectRef diffBranchRef ref =
    Link.projectBranchDefinition_
        projectRef
        diffBranchRef.ref
        (Perspective.absoluteRootPerspective diffBranchRef.hash)
        ref


branchLink : ProjectRef -> BranchDiff.DiffBranchRef -> Reference -> Html Msg -> Html Msg
branchLink projectRef diffBranchRef ref label =
    branchLink_ projectRef diffBranchRef ref
        |> Link.view_ label


viewChangeIcon : ChangeLine -> Html Msg
viewChangeIcon item =
    let
        type_ =
            ChangeLine.toString item
    in
    Tooltip.text type_
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view
            (span
                [ class "change-icon"
                , class (String.toLower type_)
                ]
                [ Icon.view Icon.largeDot ]
            )


viewDefinitionIcon : DefinitionType -> Html msg
viewDefinitionIcon definitionType =
    let
        ( description, icon ) =
            case definitionType of
                DefinitionType.Term ->
                    ( "Term", Icon.term )

                DefinitionType.Type ->
                    ( "Type", Icon.type_ )

                DefinitionType.Doc ->
                    ( "Doc", Icon.doc )

                DefinitionType.Ability ->
                    ( "Ability", Icon.ability )

                DefinitionType.AbilityConstructor ->
                    ( "Ability Constructor", Icon.abilityConstructor )

                DefinitionType.DataConstructor ->
                    ( "Data Constructor", Icon.dataConstructor )

                DefinitionType.Test ->
                    ( "Test", Icon.test )
    in
    div [ class "def-icon-anchor" ]
        [ Tooltip.text description
            |> Tooltip.tooltip
            |> Tooltip.withArrow Tooltip.Start
            |> Tooltip.view (span [ class "def-icon" ] [ Icon.view icon ])
        ]


viewDiffTreeNode : ProjectRef -> ChangedDefinitions -> ChangeLine -> Html Msg
viewDiffTreeNode projectRef changedDefinitions changeLine =
    let
        viewTitle fqn =
            Click.onClick (ExpandChangeDetails changeLine)
                |> Click.view [ class "change-title" ] [ FQN.view fqn ]

        view_ type_ content =
            div [ class "change-line" ] [ viewChangeIcon changeLine, viewDefinitionIcon type_, content ]
    in
    case changeLine of
        ChangeLine.Added type_ { shortName } ->
            view_ type_ (viewTitle shortName)

        ChangeLine.Removed type_ { shortName } ->
            view_ type_ (viewTitle shortName)

        ChangeLine.Updated type_ { shortName } ->
            view_ type_ (viewTitle shortName)

        ChangeLine.RenamedFrom type_ { newShortName } ->
            view_ type_ (viewTitle newShortName)

        ChangeLine.Aliased type_ { aliasShortName } ->
            view_ type_ (viewTitle aliasShortName)

        ChangeLine.Namespace ns ->
            div [ class "change-line namespace" ]
                (viewNamespaceLine projectRef changedDefinitions ns)


viewContributionChangesGroup : ProjectRef -> ChangedDefinitions -> List ChangeLine -> Html Msg
viewContributionChangesGroup projectRef changedDefinitions lines =
    div [ class "contribution-changes-group" ] (List.map (viewDiffTreeNode projectRef changedDefinitions) lines)


viewNamespaceLine : ProjectRef -> ChangedDefinitions -> ChangeLine.NamespaceLineItem -> List (Html Msg)
viewNamespaceLine projectRef changedDefinitions { name, lines } =
    [ div [ class "namespace-info" ] [ Icon.view Icon.folder, FQN.view name ]
    , viewContributionChangesGroup projectRef changedDefinitions lines
    ]


viewFailedToLoadExpandedContent : Http.Error -> Html msg
viewFailedToLoadExpandedContent _ =
    div [] [ text "Error, couldn't load diff details" ]


viewLoadingExpandedContent : Html msg
viewLoadingExpandedContent =
    let
        placeholder length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view
    in
    div [ class "loading-expanded-content" ]
        [ placeholder Placeholder.Small
        , placeholder Placeholder.Medium
        , placeholder Placeholder.Huge
        , placeholder Placeholder.Small
        ]


viewChangedDefinitionCard : ProjectRef -> ChangedDefinitions -> BranchDiff -> ChangeLine -> DefinitionType -> Html Msg -> Html Msg
viewChangedDefinitionCard projectRef changedDefinitions branchDiff changeLine type_ content =
    let
        linked branchRef =
            SyntaxLinked.Linked
                (Link.projectBranchDefinition projectRef branchRef)

        ( expanded, toggleIcon ) =
            case ChangedDefinitions.get changedDefinitions changeLine of
                Just { isExpanded, data } ->
                    if isExpanded then
                        case data of
                            ChangedDefinitions.Diff diff ->
                                let
                                    expandedContent =
                                        case diff of
                                            NotAsked ->
                                                viewLoadingExpandedContent

                                            Loading ->
                                                viewLoadingExpandedContent

                                            Success d ->
                                                pre [ class "monochrome" ]
                                                    [ code []
                                                        [ DefinitionDiff.view
                                                            (linked branchDiff.newBranch.ref)
                                                            d
                                                        ]
                                                    ]

                                            Failure e ->
                                                viewFailedToLoadExpandedContent e
                                in
                                ( Just expandedContent, Icon.arrowsToLine )

                            ChangedDefinitions.DefinitionSyntax syntax ->
                                let
                                    branchRef =
                                        case changeLine of
                                            ChangeLine.Removed _ _ ->
                                                branchDiff.oldBranch.ref

                                            _ ->
                                                branchDiff.newBranch.ref

                                    expandedContent =
                                        case syntax of
                                            NotAsked ->
                                                viewLoadingExpandedContent

                                            Loading ->
                                                viewLoadingExpandedContent

                                            Success s ->
                                                pre [ class "monochrome" ]
                                                    [ code [] [ Syntax.view (linked branchRef) s ] ]

                                            Failure e ->
                                                viewFailedToLoadExpandedContent e
                                in
                                ( Just expandedContent
                                , Icon.arrowsToLine
                                )

                    else
                        ( Nothing, Icon.arrowsFromLine )

                Nothing ->
                    -- Not expanded and not fetched
                    ( Nothing, Icon.arrowsFromLine )
    in
    Card.card
        [ div [ class "definition-change-header" ]
            [ div [ class "change-line" ]
                [ viewChangeIcon changeLine
                , viewDefinitionIcon type_
                , content
                ]
            , Button.icon (ToggleChangeDetails changeLine) toggleIcon
                |> Button.subdued
                |> Button.small
                |> Button.view
            ]
        , expanded
            |> Maybe.map (\details -> div [ class "definition-change-details" ] [ details ])
            |> Maybe.withDefault UI.nothing
        ]
        |> Card.withClassName ("definition-change " ++ String.toLower (ChangeLine.toString changeLine))
        |> Card.withDomId (ChangeLine.toKey changeLine)
        |> Card.asContained
        |> Card.view


viewChangedDefinitionsCards : ProjectRef -> ChangedDefinitions -> BranchDiff -> List (Html Msg)
viewChangedDefinitionsCards projectRef changedDefinitions branchDiff =
    let
        view_ =
            viewChangedDefinitionCard projectRef changedDefinitions branchDiff

        f changeLine acc =
            let
                viewTitle fqn =
                    Click.onClick (ToggleChangeDetails changeLine)
                        |> Click.view [ class "change-title" ] [ FQN.view fqn ]

                clickableHash diffBranchRef ref hash =
                    branchLink projectRef diffBranchRef ref (Hash.view hash)

                viewInfo contents =
                    div [ class "change-info" ] contents
            in
            case changeLine of
                ChangeLine.Added type_ i ->
                    view_ changeLine
                        type_
                        (viewInfo
                            [ viewTitle i.fullName
                            , clickableHash branchDiff.newBranch i.ref i.hash
                            ]
                        )
                        :: acc

                ChangeLine.Removed type_ i ->
                    view_ changeLine
                        type_
                        (viewInfo
                            [ viewTitle i.fullName
                            , clickableHash branchDiff.oldBranch i.ref i.hash
                            ]
                        )
                        :: acc

                ChangeLine.Updated type_ i ->
                    view_ changeLine
                        type_
                        (viewInfo
                            [ viewTitle i.fullName
                            , clickableHash branchDiff.oldBranch i.ref i.oldHash
                            , Icon.view Icon.arrowRight
                            , clickableHash branchDiff.newBranch i.ref i.newHash
                            ]
                        )
                        :: acc

                ChangeLine.RenamedFrom type_ i ->
                    view_ changeLine
                        type_
                        (viewInfo
                            [ viewTitle i.newFullName
                            , clickableHash branchDiff.oldBranch i.newRef i.hash
                            , span [ class "extra-info" ]
                                [ text "(was "
                                , branchLink projectRef
                                    branchDiff.oldBranch
                                    -- TODO: should be oldRefs, plural...
                                    i.oldRef
                                    (i.oldNames
                                        |> NEL.map FQN.toString
                                        |> NEL.toList
                                        |> String.join ", "
                                        |> text
                                    )
                                , text ")"
                                ]
                            ]
                        )
                        :: acc

                ChangeLine.Aliased type_ i ->
                    view_ changeLine
                        type_
                        (viewInfo
                            [ viewTitle i.aliasFullName
                            , clickableHash branchDiff.newBranch i.ref i.hash
                            , span [ class "extra-info" ]
                                [ text "(AKA "
                                , i.otherNames
                                    |> NEL.map FQN.toString
                                    |> NEL.toList
                                    |> String.join ", "
                                    |> text
                                , text ")"
                                ]
                            ]
                        )
                        :: acc

                ChangeLine.Namespace ns ->
                    go ns.lines ++ acc

        go lines =
            List.foldr f [] lines
    in
    go branchDiff.lines


viewBranchDiff : AppContext -> ProjectRef -> ChangedDefinitions -> BranchDiff -> Html Msg
viewBranchDiff _ projectRef changedDefinitions diff =
    let
        summary =
            BranchDiff.summary diff

        -- There's no reason to show a tree with a single element...
        tree =
            if BranchDiff.size diff > 1 then
                Card.card
                    [ viewContributionChangesGroup projectRef changedDefinitions diff.lines
                    ]
                    |> Card.withClassName "change-tree"
                    |> Card.asContained
                    |> Card.view

            else
                UI.nothing
    in
    div [ class "branch-diff-content" ]
        [ strong []
            [ text (pluralize "change" "changes" summary.numChanges)
            , text " across "
            , text (pluralize "namespace" "namespaces" summary.numNamespaceChanges)
            ]
        , div [ class "branch-diff-content-cards" ]
            [ tree
            , div [ id "definition-changes", class "definition-changes" ]
                (viewChangedDefinitionsCards projectRef changedDefinitions diff)
            ]
        ]


viewLoadingPage : PageContent Msg
viewLoadingPage =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view
    in
    PageContent.oneColumn
        [ div [ class "project-contribution-changes-page" ]
            [ Card.card
                [ shape Placeholder.Large
                , shape Placeholder.Small
                , shape Placeholder.Medium
                ]
                |> Card.asContained
                |> Card.view
            ]
        ]


viewErrorPage : ContributionRef -> Http.Error -> PageContent Msg
viewErrorPage _ _ =
    PageContent.oneColumn
        [ div [ class "project-contribution-changes-page" ]
            [ StatusBanner.bad
                "Something broke on our end and we couldn't show the contribution changes. Please try again."
            ]
        ]


view : AppContext -> ProjectRef -> Contribution -> Model -> PageContent Msg
view appContext projectRef contribution model =
    case model.branchDiff of
        NotAsked ->
            viewLoadingPage

        Loading ->
            viewLoadingPage

        Success diff ->
            PageContent.oneColumn
                [ TabList.tabList
                    [ TabList.tab "Overview" (Link.projectContribution projectRef contribution.ref)
                    ]
                    (TabList.tab "Changes" (Link.projectContributionChanges projectRef contribution.ref))
                    []
                    |> TabList.view
                , div [ class "project-contribution-changes-page" ]
                    [ viewBranchDiff appContext
                        projectRef
                        model.changedDefinitions
                        diff
                    ]
                ]

        Failure e ->
            viewErrorPage contribution.ref e
