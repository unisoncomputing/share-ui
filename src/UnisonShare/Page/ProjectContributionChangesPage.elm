port module UnisonShare.Page.ProjectContributionChangesPage exposing (..)

import Code.BranchRef as BranchRef
import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Perspective as Perspective
import Code.Syntax as Syntax
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Html exposing (Html, br, code, div, h2, label, p, pre, span, strong, text)
import Html.Attributes exposing (class, id, style)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.ScrollTo as ScrollTo
import Lib.Util as Util
import List.Nonempty as NEL
import String.Extra exposing (pluralize)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon exposing (Icon)
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
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType)
import UnisonShare.BranchDiff.LibDep as LibDep exposing (LibDep)
import UnisonShare.BranchDiff.ToggledChangeLines as ToggledChangeLines exposing (ToggledChangeLines)
import UnisonShare.BranchDiffState as BranchDiffState exposing (BranchDiffState)
import UnisonShare.Contribution exposing (ContributionDetails)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.DefinitionDiff as DefinitionDiff
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import UnisonShare.Session as Session
import Url



-- MODEL


type alias Model =
    { branchDiff : BranchDiffState
    , toggledChangeLines : ToggledChangeLines
    , urlFocusedChangeLineId : Maybe ChangeLineId
    }


type alias DiffBranches =
    { oldBranch : BranchDiff.DiffBranchRef
    , newBranch : BranchDiff.DiffBranchRef
    }


init : AppContext -> ProjectRef -> ContributionRef -> Maybe ChangeLineId -> ( Model, Cmd Msg )
init appContext projectRef contribRef changeLineId =
    ( { branchDiff = BranchDiffState.Loading
      , toggledChangeLines = ToggledChangeLines.empty
      , urlFocusedChangeLineId = changeLineId
      }
    , fetchBranchDiff appContext projectRef contribRef 1
    )



-- UPDATE


type Msg
    = FetchBranchDiffFinished (HttpResult BranchDiffState)
    | RetryBranchDiffFetch Int
    | ToggleChangeDetails ChangeLine
    | CopyChangeLinePermalink ChangeLineId
    | SetChangeLinePermalink ChangeLineId
    | ScrollTo ChangeLineId
    | NoOp


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef contribRef msg model =
    case msg of
        FetchBranchDiffFinished branchDiffState ->
            let
                ( model_, cmd ) =
                    case branchDiffState of
                        Ok (BranchDiffState.Computed bd) ->
                            let
                                toggledChangeLines =
                                    model.urlFocusedChangeLineId
                                        |> Maybe.andThen (\changeLineId -> BranchDiff.changeLineById changeLineId bd)
                                        |> Maybe.map
                                            (\changeLine ->
                                                ToggledChangeLines.expand model.toggledChangeLines changeLine
                                            )
                                        |> Maybe.withDefault model.toggledChangeLines

                                cmd_ =
                                    model.urlFocusedChangeLineId
                                        |> Maybe.map (ScrollTo >> Util.delayMsg 10)
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( { model
                                | branchDiff =
                                    BranchDiffState.Computed
                                        { bd | lines = BranchDiff.condense bd.lines }
                                , toggledChangeLines = toggledChangeLines
                              }
                            , cmd_
                            )

                        Ok ((BranchDiffState.Computing { numTries }) as bds) ->
                            if numTries >= 3 then
                                ( { model | branchDiff = bds }, Cmd.none )

                            else
                                ( { model | branchDiff = BranchDiffState.Reloading { numTries = numTries + 1 } }
                                , Util.delayMsg 1000 (RetryBranchDiffFetch (numTries + 1))
                                )

                        Ok bds ->
                            ( { model | branchDiff = bds }, Cmd.none )

                        Err e ->
                            ( { model | branchDiff = BranchDiffState.Failure e }, Cmd.none )
            in
            ( model_, cmd )

        RetryBranchDiffFetch numTries ->
            ( model
            , fetchBranchDiff appContext projectRef contribRef numTries
            )

        ScrollTo changeLineId ->
            ( model, scrollTo changeLineId )

        ToggleChangeDetails changeLine ->
            case model.branchDiff of
                BranchDiffState.Computed _ ->
                    let
                        toggled =
                            if ToggledChangeLines.isExpanded model.toggledChangeLines changeLine then
                                ToggledChangeLines.collapse model.toggledChangeLines changeLine

                            else
                                ToggledChangeLines.expand model.toggledChangeLines changeLine
                    in
                    ( { model | toggledChangeLines = toggled }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SetChangeLinePermalink changeLineId ->
            let
                route =
                    changeLineId
                        |> Route.projectContributionChange projectRef contribRef
            in
            ( model
            , Cmd.batch
                [ Route.navigate appContext.navKey route
                , scrollTo changeLineId
                ]
            )

        CopyChangeLinePermalink changeLineId ->
            let
                route =
                    changeLineId
                        |> Route.projectContributionChange projectRef contribRef

                url =
                    route
                        |> Route.toUrl appContext
                        |> Url.toString
            in
            ( model
            , Cmd.batch
                [ copyToClipboard url
                , Route.navigate appContext.navKey route
                , scrollTo changeLineId
                ]
            )

        NoOp ->
            ( model, Cmd.none )


port copyToClipboard : String -> Cmd msg



-- EFFECTS


scrollTo : ChangeLineId -> Cmd Msg
scrollTo changeLineId =
    ScrollTo.scrollTo_ NoOp "page-content" (ChangeLineId.toDomId changeLineId) 16


fetchBranchDiff : AppContext -> ProjectRef -> ContributionRef -> Int -> Cmd Msg
fetchBranchDiff appContext projectRef contributionRef numTries =
    ShareApi.projectContributionDiff projectRef contributionRef
        |> HttpApi.toRequest (BranchDiffState.decode numTries) FetchBranchDiffFinished
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


changeIcon : ChangeLine -> Icon msg
changeIcon changeLine =
    case changeLine of
        ChangeLine.Added _ _ ->
            Icon.largePlus

        ChangeLine.Removed _ _ ->
            Icon.dash

        ChangeLine.Updated _ _ ->
            Icon.refreshSmallBold

        ChangeLine.RenamedFrom _ _ ->
            Icon.tag

        ChangeLine.Aliased _ _ ->
            Icon.tags

        _ ->
            Icon.largeDot


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
                [ Icon.view (changeIcon item) ]
            )


viewChangeBadge : Int -> ChangeLine -> Html Msg
viewChangeBadge maxBadgeLength changeLine =
    let
        type_ =
            ChangeLine.toString changeLine

        width =
            String.fromInt maxBadgeLength
    in
    span [ class "change-badge_wrapper", style "width" (width ++ "ch") ]
        [ span
            [ class "change-badge"
            , class (String.toLower type_)
            ]
            [ Icon.view (changeIcon changeLine)
            , label [] [ text type_ ]
            ]
        ]


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


viewDiffTreeNode : ProjectRef -> ToggledChangeLines -> ChangeLine -> Html Msg
viewDiffTreeNode projectRef toggledChangeLines changeLine =
    let
        viewTitle fqn =
            case ChangeLine.toChangeLineId changeLine of
                Just changeLineId ->
                    Click.onClick (SetChangeLinePermalink changeLineId)
                        |> Click.view [ class "change-title" ] [ FQN.view fqn ]

                Nothing ->
                    div [ class "change-title" ] [ FQN.view fqn ]

        view_ type_ content =
            div [ class "change-line" ]
                [ viewChangeIcon changeLine
                , viewDefinitionIcon type_
                , content
                ]
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
            viewNamespaceLine projectRef toggledChangeLines ns


viewContributionChangesGroup : ProjectRef -> ToggledChangeLines -> List ChangeLine -> Html Msg
viewContributionChangesGroup projectRef toggledChangeLines lines =
    div [ class "contribution-changes-group" ] (List.map (viewDiffTreeNode projectRef toggledChangeLines) lines)


viewNamespaceLine : ProjectRef -> ToggledChangeLines -> ChangeLine.NamespaceLineItem -> Html Msg
viewNamespaceLine projectRef toggledChangeLines { name, lines } =
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
            , viewContributionChangesGroup projectRef toggledChangeLines lines
            ]


viewChangedDefinitionCard : ProjectRef -> ToggledChangeLines -> BranchDiff -> Int -> ChangeLine -> DefinitionType -> Html Msg -> Html Msg
viewChangedDefinitionCard projectRef toggledChangeLines branchDiff maxBadgeLength changeLine type_ content =
    let
        toSyntaxConfig isNew =
            let
                diffBranchRef =
                    if isNew then
                        branchDiff.newBranch

                    else
                        branchDiff.oldBranch
            in
            SyntaxConfig.empty
                |> SyntaxConfig.withToClick
                    (Link.projectBranchDefinition_
                        projectRef
                        diffBranchRef.ref
                        (Perspective.absoluteRootPerspective diffBranchRef.hash)
                    )

        ( expanded, toggleIcon ) =
            if ToggledChangeLines.isCollapsed toggledChangeLines changeLine then
                ( Nothing, Icon.arrowsFromLine )

            else
                case changeLine of
                    ChangeLine.Updated _ { diff } ->
                        ( Just (DefinitionDiff.view toSyntaxConfig diff), Icon.arrowsToLine )

                    _ ->
                        case ChangeLine.source changeLine of
                            Just source ->
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
                                        pre [ class "definition-syntax monochrome" ]
                                            [ code [] [ Syntax.view linked source ] ]
                                in
                                ( Just expandedContent
                                , Icon.arrowsToLine
                                )

                            Nothing ->
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
                [ viewChangeBadge maxBadgeLength changeLine
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


viewChangedDefinitionsCards : ProjectRef -> ToggledChangeLines -> BranchDiff -> List (Html Msg)
viewChangedDefinitionsCards projectRef toggledChangeLines branchDiff =
    let
        maxBadgeLength =
            branchDiff.lines
                |> List.map (ChangeLine.toString >> String.length)
                |> List.maximum
                |> Maybe.withDefault 0

        view_ =
            viewChangedDefinitionCard
                projectRef
                toggledChangeLines
                branchDiff
                maxBadgeLength

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

        changeIcon_ type_ icon =
            Tooltip.text type_
                |> Tooltip.tooltip
                |> Tooltip.withArrow Tooltip.Start
                |> Tooltip.view
                    (span
                        [ class "change-icon"
                        , class (String.toLower type_)
                        ]
                        [ Icon.view icon ]
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
                [ changeIcon_ "Added" Icon.largePlus
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
                [ changeIcon_ "Removed" Icon.dash
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


viewBranchDiff : ProjectRef -> ToggledChangeLines -> BranchDiff -> Html Msg
viewBranchDiff projectRef toggledChangeLines diff =
    let
        summary =
            BranchDiff.summary diff

        -- There's no reason to show a tree with a single element...
        tree =
            if BranchDiff.size diff > 1 then
                Card.card
                    [ viewContributionChangesGroup projectRef toggledChangeLines diff.lines
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
                    ++ viewChangedDefinitionsCards projectRef toggledChangeLines diff
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
                        pre [] [ text (Util.httpErrorToString err) ]

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
                    [ viewBranchDiff projectRef model.toggledChangeLines diff ]
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
