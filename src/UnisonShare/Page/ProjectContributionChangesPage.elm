port module UnisonShare.Page.ProjectContributionChangesPage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Perspective as Perspective
import Code.Syntax as Syntax
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Html exposing (Html, br, code, div, h2, p, pre, span, strong, text)
import Html.Attributes exposing (class, id)
import Http
import Json.Decode as Decode
import Lib.Decode.Helpers exposing (failInvalid)
import Lib.HttpApi as HttpApi exposing (HttpResult)
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
import UI.StatusIndicator as StatusIndicator
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Account as Account
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchDiff as BranchDiff exposing (BranchDiff)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId exposing (ChangeLineId)
import UnisonShare.BranchDiff.ChangedDefinitions as ChangedDefinitions exposing (ChangedDefinitions)
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType)
import UnisonShare.BranchDiff.LibDep as LibDep exposing (LibDep)
import UnisonShare.BranchDiffState as BranchDiffState exposing (BranchDiffState)
import UnisonShare.Contribution exposing (ContributionDetails)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import UnisonShare.Session as Session
import Url



-- MODEL


type alias Model =
    { branchDiff : BranchDiffState
    , changedDefinitions : ChangedDefinitions
    , urlFocusedChangeLineId : Maybe ChangeLineId
    }


type alias DiffBranches =
    { oldBranch : BranchDiff.DiffBranchRef
    , newBranch : BranchDiff.DiffBranchRef
    }


init : AppContext -> ProjectRef -> ContributionRef -> Maybe ChangeLineId -> ( Model, Cmd Msg )
init appContext projectRef contribRef changeLineId =
    ( { branchDiff = BranchDiffState.Loading
      , changedDefinitions = ChangedDefinitions.empty
      , urlFocusedChangeLineId = changeLineId
      }
    , fetchBranchDiff appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = FetchBranchDiffFinished (HttpResult BranchDiffState)
    | ExpandChangeDetails ChangeLine
    | ToggleChangeDetails ChangeLine
    | FetchDefinitionDiffFinished ChangeLine (WebData DefinitionDiff)
    | FetchDefinitionSyntaxFinished ChangeLine (WebData Syntax.Syntax)
    | CopyChangeLinePermalink ChangeLineId
    | NoOp


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef contribRef msg model =
    case msg of
        FetchBranchDiffFinished branchDiffState ->
            let
                ( branchDiffState_, cmd ) =
                    case branchDiffState of
                        Ok (BranchDiffState.Computed bd) ->
                            let
                                expandCmd =
                                    model.urlFocusedChangeLineId
                                        |> Maybe.andThen (\changeLineId -> BranchDiff.changeLineById changeLineId bd)
                                        |> Maybe.map (\changeLine -> Util.delayMsg 500 (ExpandChangeDetails changeLine))
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( BranchDiffState.Computed { bd | lines = BranchDiff.condense bd.lines }, expandCmd )

                        Ok bds ->
                            ( bds, Cmd.none )

                        Err e ->
                            ( BranchDiffState.Failure e, Cmd.none )
            in
            ( { model | branchDiff = branchDiffState_ }, cmd )

        ExpandChangeDetails changeLine ->
            case model.branchDiff of
                BranchDiffState.Computed branchDiff ->
                    expandAndScrollTo appContext
                        projectRef
                        contribRef
                        model
                        branchDiff
                        changeLine

                _ ->
                    ( model, Cmd.none )

        ToggleChangeDetails changeLine ->
            case model.branchDiff of
                BranchDiffState.Computed branchDiff ->
                    if ChangedDefinitions.isExpanded model.changedDefinitions changeLine then
                        ( { model
                            | changedDefinitions =
                                ChangedDefinitions.collapse model.changedDefinitions changeLine
                          }
                          -- Clear selected changeline from the URL
                        , Route.navigate appContext.navKey (Route.projectContributionChanges projectRef contribRef)
                        )

                    else
                        expandAndScrollTo appContext projectRef contribRef model branchDiff changeLine

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
                        , data = ChangedDefinitions.Diff resp
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

        CopyChangeLinePermalink changeLineId ->
            let
                url =
                    changeLineId
                        |> Route.projectContributionChange projectRef contribRef
                        |> Route.toUrl appContext
                        |> Url.toString
            in
            ( model, copyToClipboard url )

        NoOp ->
            ( model, Cmd.none )


port copyToClipboard : String -> Cmd msg



-- UPDATE HELPERS


expandAndScrollTo : AppContext -> ProjectRef -> ContributionRef -> Model -> BranchDiff -> ChangeLine -> ( Model, Cmd Msg )
expandAndScrollTo appContext projectRef contribRef model branchDiff changeLine =
    let
        navCmd =
            changeLine
                |> ChangeLine.toChangeLineId
                |> Maybe.map (Route.projectContributionChange projectRef contribRef)
                |> Maybe.map (Route.navigate appContext.navKey)
                |> Maybe.withDefault Cmd.none
    in
    case ChangeLine.toChangeLineId changeLine of
        Nothing ->
            ( model, Cmd.none )

        Just changeLineId ->
            if ChangedDefinitions.isLoaded model.changedDefinitions changeLine then
                ( { model
                    | changedDefinitions =
                        ChangedDefinitions.expand model.changedDefinitions changeLine
                  }
                , Cmd.batch [ navCmd, scrollTo changeLineId ]
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
                            contribRef
                            DefinitionDiff.Term
                            changeLine
                            { old = name
                            , new = name
                            }

                    fetchTypeDefinitionDiff name =
                        fetchDefinitionDiff appContext
                            projectRef
                            contribRef
                            DefinitionDiff.Type
                            changeLine
                            { old = name
                            , new = name
                            }

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
                    , navCmd
                    , scrollTo changeLineId
                    ]
                )


scrollTo : ChangeLineId -> Cmd Msg
scrollTo changeLineId =
    ScrollTo.scrollTo_ NoOp "page-content" (ChangeLineId.toDomId changeLineId) 16



-- EFFECTS


fetchBranchDiff : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchBranchDiff appContext projectRef contributionRef =
    ShareApi.projectContributionDiff projectRef contributionRef
        |> HttpApi.toRequest (BranchDiffState.decode 1) FetchBranchDiffFinished
        |> HttpApi.perform appContext.api


fetchDefinitionDiff :
    AppContext
    -> ProjectRef
    -> ContributionRef
    -> DefinitionDiff.DefinitionType
    -> ChangeLine
    -> { old : FQN.FQN, new : FQN.FQN }
    -> Cmd Msg
fetchDefinitionDiff appContext projectRef contribRef definitionType changeLine params =
    ShareApi.projectContributionDefinitionDiff projectRef contribRef definitionType params
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
                    |> Decode.andThen (failInvalid "No valid definitions returned")
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

        ChangeLine.Propagated _ _ ->
            UI.nothing

        ChangeLine.RenamedFrom type_ { newShortName } ->
            view_ type_ (viewTitle newShortName)

        ChangeLine.Aliased type_ { aliasShortName } ->
            view_ type_ (viewTitle aliasShortName)

        ChangeLine.Namespace ns ->
            viewNamespaceLine projectRef changedDefinitions ns


viewContributionChangesGroup : ProjectRef -> ChangedDefinitions -> List ChangeLine -> Html Msg
viewContributionChangesGroup projectRef changedDefinitions lines =
    div [ class "contribution-changes-group" ] (List.map (viewDiffTreeNode projectRef changedDefinitions) lines)


viewNamespaceLine : ProjectRef -> ChangedDefinitions -> ChangeLine.NamespaceLineItem -> Html Msg
viewNamespaceLine projectRef changedDefinitions { name, lines } =
    let
        isDeeplyPropagated cl =
            case cl of
                ChangeLine.Propagated _ _ ->
                    True

                ChangeLine.Namespace ns ->
                    List.all isDeeplyPropagated ns.lines

                _ ->
                    False
    in
    if List.all isDeeplyPropagated lines then
        UI.nothing

    else
        div [ class "change-line namespace" ]
            [ div [ class "namespace-info" ] [ Icon.view Icon.folder, FQN.view name ]
            , viewContributionChangesGroup projectRef changedDefinitions lines
            ]


viewFailedToLoadExpandedContent : Http.Error -> Html msg
viewFailedToLoadExpandedContent _ =
    div [ class "error-expanded-content" ]
        [ text "Error, couldn't load diff details"

        -- , div [] [ text (Util.httpErrorToString e) ]
        ]


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
        toSyntaxConfig isNew =
            let
                branchRef =
                    if isNew then
                        branchDiff.newBranch.ref

                    else
                        branchDiff.oldBranch.ref
            in
            SyntaxConfig.empty
                |> SyntaxConfig.withToClick
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
                                                DefinitionDiff.view
                                                    toSyntaxConfig
                                                    d

                                            Failure e ->
                                                viewFailedToLoadExpandedContent e
                                in
                                ( Just expandedContent, Icon.arrowsToLine )

                            ChangedDefinitions.DefinitionSyntax syntax ->
                                let
                                    linked =
                                        let
                                            branchRef =
                                                case changeLine of
                                                    ChangeLine.Removed _ _ ->
                                                        branchDiff.oldBranch.ref

                                                    _ ->
                                                        branchDiff.newBranch.ref
                                        in
                                        SyntaxConfig.empty
                                            |> SyntaxConfig.withToClick
                                                (Link.projectBranchDefinition projectRef branchRef)

                                    expandedContent =
                                        case syntax of
                                            NotAsked ->
                                                viewLoadingExpandedContent

                                            Loading ->
                                                viewLoadingExpandedContent

                                            Success s ->
                                                pre [ class "definition-syntax monochrome" ]
                                                    [ code [] [ Syntax.view linked s ] ]

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

        domId =
            changeLine
                |> ChangeLine.toChangeLineId
                |> Maybe.map ChangeLineId.toDomId

        copyPermalink =
            changeLine
                |> ChangeLine.toChangeLineId
                |> Maybe.map
                    (\cid ->
                        Button.icon (CopyChangeLinePermalink cid) Icon.chain
                            |> Button.subdued
                            |> Button.small
                            |> Button.view
                    )
                |> Maybe.map
                    (\bt ->
                        Tooltip.text "Copy permanent link"
                            |> Tooltip.tooltip
                            |> Tooltip.withPosition Tooltip.LeftOf
                            |> Tooltip.view bt
                    )
    in
    Card.card
        [ div [ class "definition-change-header" ]
            [ div [ class "change-line" ]
                [ viewChangeIcon changeLine
                , viewDefinitionIcon type_
                , content
                ]
            , div [ class "definition-change-actions" ]
                [ Maybe.withDefault UI.nothing copyPermalink
                , Button.icon (ToggleChangeDetails changeLine) toggleIcon
                    |> Button.subdued
                    |> Button.small
                    |> Button.view
                ]
            ]
        , expanded
            |> Maybe.map
                (\details ->
                    div [ class "definition-change-details" ] [ details ]
                )
            |> Maybe.withDefault UI.nothing
        ]
        |> Card.withClassName
            ("definition-change "
                ++ String.toLower
                    (ChangeLine.toString changeLine)
            )
        |> Util.pipeMaybe Card.withDomId domId
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

                ChangeLine.Propagated _ _ ->
                    acc

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


viewLibDep : LibDep -> Html msg
viewLibDep dep =
    let
        viewCard content =
            Card.card
                [ div [ class "definition-change-header" ] [ div [ class "change-line" ] content ] ]
                |> Card.withClassName "definition-change lib-dep"
                |> Card.asContained
                |> Card.view

        changeIcon type_ =
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

        viewTitle name =
            let
                fqn =
                    FQN.fromList [ "lib", name ]
            in
            div [ class "change-title" ] [ FQN.view fqn ]
    in
    case dep of
        LibDep.Added { name } ->
            viewCard
                [ changeIcon "Added"
                , div [ class "def-icon-anchor" ]
                    [ Tooltip.text "Lib dependency"
                        |> Tooltip.tooltip
                        |> Tooltip.withArrow Tooltip.Start
                        |> Tooltip.view (span [ class "def-icon" ] [ Icon.view Icon.book ])
                    ]
                , div [ class "change-info" ] [ viewTitle name ]
                ]

        LibDep.Removed { name } ->
            viewCard
                [ changeIcon "Removed"
                , div [ class "def-icon-anchor" ]
                    [ Tooltip.text "Lib dependency"
                        |> Tooltip.tooltip
                        |> Tooltip.withArrow Tooltip.Start
                        |> Tooltip.view (span [ class "def-icon" ] [ Icon.view Icon.book ])
                    ]
                , div [ class "change-info" ] [ viewTitle name ]
                ]


viewLibDeps : List LibDep -> List (Html msg)
viewLibDeps deps =
    List.map viewLibDep deps


viewBranchDiff : ProjectRef -> ChangedDefinitions -> BranchDiff -> Html Msg
viewBranchDiff projectRef changedDefinitions diff =
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
            ]
        , div [ class "branch-diff-content-cards" ]
            [ tree
            , div [ id "definition-changes", class "definition-changes" ]
                (viewLibDeps diff.libDeps
                    ++ viewChangedDefinitionsCards projectRef changedDefinitions diff
                )
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


viewErrorPage : AppContext -> ContributionRef -> Http.Error -> PageContent Msg
viewErrorPage appContext _ err =
    let
        errorDetails =
            case appContext.session of
                Session.SignedIn account ->
                    if Account.isUnisonMember account then
                        text (Util.httpErrorToString err)

                    else
                        UI.nothing

                _ ->
                    UI.nothing
    in
    PageContent.oneColumn
        [ div [ class "project-contribution-changes-page" ]
            [ StatusBanner.bad
                "Something broke on our end and we couldn't show the contribution changes. Please try again."
            , errorDetails
            ]
        ]


viewCulprit : ContributionDetails -> BranchDiffState.DiffErrorCulprit -> Html msg
viewCulprit contribution culprit =
    case culprit of
        BranchDiffState.TargetBranch ->
            strong [] [ text (BranchRef.toString contribution.targetBranchRef) ]

        BranchDiffState.SourceBranch ->
            strong [] [ text (BranchRef.toString contribution.sourceBranchRef) ]


view : AppContext -> ProjectRef -> ContributionDetails -> Model -> PageContent Msg
view appContext projectRef contribution model =
    let
        tabs =
            TabList.tabList
                [ TabList.tab "Overview" (Link.projectContribution projectRef contribution.ref)
                ]
                (TabList.tab "Changes" (Link.projectContributionChanges projectRef contribution.ref))
                []
                |> TabList.view
    in
    case model.branchDiff of
        BranchDiffState.Loading ->
            viewLoadingPage

        BranchDiffState.Computing _ ->
            PageContent.oneColumn
                [ tabs
                , div [ class "project-contribution-changes-page" ]
                    [ Card.card
                        [ StatusBanner.working "The contribution diff is still being computed..."
                        , p [ class "refresh-message" ]
                            [ text "Unison Share will never show a stale diff, and sometimes it takes a minute or two to compute."
                            , br [] []
                            , text "You can try refreshing this page."
                            ]
                        ]
                        |> Card.withClassName "contribution-diff_computing"
                        |> Card.asContainedWithFade
                        |> Card.view
                    ]
                ]

        BranchDiffState.Reloading _ ->
            viewLoadingPage

        BranchDiffState.Computed diff ->
            PageContent.oneColumn
                [ tabs
                , div
                    [ class "project-contribution-changes-page" ]
                    [ viewBranchDiff projectRef model.changedDefinitions diff ]
                ]

        BranchDiffState.Uncomputable error ->
            let
                errorDetails =
                    case error of
                        BranchDiffState.ConstructorAlias { culprit, typeName, constructorName1, constructorName2 } ->
                            Just
                                [ text "The type "
                                , FQN.view typeName
                                , text " has a constructor alias: "
                                , FQN.view constructorName1
                                , text " and "
                                , FQN.view constructorName2
                                , text " on the "
                                , viewCulprit contribution culprit
                                , text " branch."
                                ]

                        BranchDiffState.MissingConstructorName { culprit, typeName } ->
                            Just
                                [ text "The type "
                                , FQN.view typeName
                                , text " is missing a constructor name on the "
                                , viewCulprit contribution culprit
                                , text " branch."
                                ]

                        BranchDiffState.NestedDeclAlias { culprit, constructorName1, constructorName2 } ->
                            Just
                                [ text "On the "
                                , viewCulprit contribution culprit
                                , text " branch, the type "
                                , FQN.view constructorName1
                                , text " is an alias of "
                                , FQN.view constructorName2
                                , text "."
                                , br [] []
                                , text "It's nested under an alias of itself. Please separate them or delete one copy."
                                ]

                        BranchDiffState.StrayConstructor { culprit, constructorName } ->
                            Just
                                [ text "The constructor "
                                , FQN.view constructorName
                                , text " is orphaned on the "
                                , viewCulprit contribution culprit
                                , text " branch."
                                ]

                        BranchDiffState.LibFoundAtUnexpectedPath ->
                            Just
                                [ text "lib namespace found at unexpected path."
                                ]

                        _ ->
                            Nothing
            in
            PageContent.oneColumn
                [ tabs
                , div [ class "project-contribution-changes-page" ]
                    [ Card.card
                        [ h2 []
                            [ StatusIndicator.bad |> StatusIndicator.view
                            , text "Unfortunately the contribution diff could not be computed."
                            ]
                        , errorDetails
                            |> Maybe.map (p [])
                            |> Maybe.withDefault UI.nothing
                        ]
                        |> Card.withClassName "contribution-diff_uncomputable"
                        |> Card.asContainedWithFade
                        |> Card.view
                    ]
                ]

        BranchDiffState.Failure e ->
            viewErrorPage appContext contribution.ref e
