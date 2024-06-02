{- Various shared setup for pages that include codebase browsing, user page,
   codebase page, project page.
-}


module UnisonShare.Page.CodePageContent exposing (..)

import Code.CodebaseTree as CodebaseTree
import Code.Config exposing (Config)
import Code.Definition.Readme exposing (Readme)
import Code.EmptyState as EmptyState
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Namespace as Namespace exposing (NamespaceDetails)
import Code.Perspective as Perspective exposing (Perspective)
import Code.ReadmeCard as ReadmeCard
import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class, classList)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.ErrorCard as ErrorCard
import UI.Icon as Icon
import UI.Placeholder as Placeholder
import UI.Sidebar as Sidebar exposing (Sidebar)
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)



-- EFFECTS


fetchPerspectiveAndCodebaseTree :
    AppContext
    -> Config
    -> (FQN -> WebData NamespaceDetails -> msg)
    -> (CodebaseTree.Msg -> msg)
    -> CodeBrowsingContext
    -> Perspective
    -> { m | codebaseTree : CodebaseTree.Model }
    -> ( { m | codebaseTree : CodebaseTree.Model }, Cmd msg )
fetchPerspectiveAndCodebaseTree appContext config finishedMsg codebaseTreeMsg context oldPerspective model =
    let
        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config

        fetchNamespaceDetailsCmd =
            config.perspective
                |> fetchNamespaceDetails finishedMsg context
                |> Maybe.map (HttpApi.perform appContext.api)
                |> Maybe.withDefault Cmd.none
    in
    if not (Perspective.equals oldPerspective config.perspective) && Perspective.needsFetching config.perspective then
        ( { model | codebaseTree = codebaseTree }
        , Cmd.batch
            [ Cmd.map codebaseTreeMsg codebaseTreeCmd
            , fetchNamespaceDetailsCmd
            ]
        )

    else if not (Perspective.equals oldPerspective config.perspective) then
        ( { model | codebaseTree = codebaseTree }, Cmd.map codebaseTreeMsg codebaseTreeCmd )

    else
        ( model, Cmd.none )


fetchNamespaceDetails :
    (FQN -> WebData NamespaceDetails -> msg)
    -> CodeBrowsingContext
    -> Perspective
    -> Maybe (HttpApi.ApiRequest NamespaceDetails msg)
fetchNamespaceDetails finishedMsg context perspective =
    let
        fqn_ =
            case perspective of
                Perspective.Namespace { fqn } ->
                    fqn

                _ ->
                    FQN.root
    in
    fqn_
        |> ShareApi.namespace context perspective
        |> HttpApi.toRequest Namespace.decodeDetails (RemoteData.fromResult >> finishedMsg fqn_)
        |> Just



-- VIEW


viewLoading : Html msg
viewLoading =
    [ Placeholder.text |> Placeholder.view
    , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
    , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
    ]
        |> Card.titled "Loading..."
        |> Card.view


viewError : Http.Error -> Html msg
viewError _ =
    ErrorCard.empty |> ErrorCard.view


viewReadme :
    (ReadmeCard.Msg -> msg)
    -> ReadmeCard.Model
    -> Maybe Readme
    -> Html msg
    -> Html msg
viewReadme readmeCardMsg readmeCard readme emptyState =
    readme
        |> Maybe.map (ReadmeCard.view readmeCard)
        |> Maybe.map (Html.map readmeCardMsg)
        |> Maybe.withDefault emptyState


viewNamespacePerspectiveHeader : (FQN -> msg) -> { a | fqn : FQN, details : WebData NamespaceDetails } -> Html msg
viewNamespacePerspectiveHeader changePerspectiveToNamespaceMsg { fqn } =
    let
        -- Imprecise, but close enough, approximation of overflowing,
        -- which results in a slight faded left edge A better way would
        -- be to measure the DOM like we do for overflowing docs, but
        -- thats quite involved...
        isOverflowing =
            fqn |> FQN.toString |> String.length |> (\l -> l > 20)

        toClick fqn_ =
            Click.onClick (changePerspectiveToNamespaceMsg fqn_)
    in
    div [ classList [ ( "namespace-header", True ), ( "is-overflowing", isOverflowing ) ] ]
        [ Icon.view Icon.folderOutlined
        , h3 [ class "namespace" ] [ FQN.viewClickable toClick fqn ]
        ]


viewPerspectiveHeader :
    (FQN -> msg)
    -> msg
    -> Perspective
    -> Maybe (Sidebar.SidebarHeader msg)
viewPerspectiveHeader changePerspectiveToNamespaceMsg upOneLevelMsg perspective =
    case perspective of
        Perspective.Root _ ->
            Nothing

        Perspective.Namespace ns ->
            let
                upToDestination =
                    if FQN.numSegments ns.fqn == 1 then
                        text "Overview"

                    else
                        FQN.dropLast ns.fqn |> FQN.view
            in
            [ viewNamespacePerspectiveHeader changePerspectiveToNamespaceMsg ns
            , div [ class "perspective-actions" ]
                [ Tooltip.tooltip
                    (Tooltip.rich upToDestination)
                    |> Tooltip.withArrow Tooltip.Start
                    |> Tooltip.view
                        (Button.icon upOneLevelMsg
                            Icon.arrowLeftUp
                            |> Button.small
                            |> Button.view
                        )
                ]
            , UI.divider
            ]
                |> Sidebar.SidebarHeader
                |> Just


viewSidebar :
    Perspective
    ->
        { upOneLevelMsg : msg
        , showFinderModalMsg : msg
        , changePerspectiveToNamespaceMsg : FQN -> msg
        }
    -> Maybe { codebaseTree : CodebaseTree.Model, codebaseTreeMsg : CodebaseTree.Msg -> msg }
    -> Sidebar msg
viewSidebar perspective cfg codebaseTree =
    let
        perspectiveHeader =
            viewPerspectiveHeader
                cfg.changePerspectiveToNamespaceMsg
                cfg.upOneLevelMsg
                perspective

        codeSection =
            Maybe.map
                (\c ->
                    Sidebar.section "Code" [ Html.map c.codebaseTreeMsg (CodebaseTree.view c.codebaseTree) ]
                        |> Sidebar.sectionWithTitleButton (Button.iconThenLabel cfg.showFinderModalMsg Icon.browse "Search" |> Button.small)
                        |> Sidebar.sectionWithScrollable
                )
                codebaseTree

        sidebar =
            case codeSection of
                Just cs ->
                    Sidebar.sidebar_ "main-sidebar" perspectiveHeader
                        |> Sidebar.withSection cs

                Nothing ->
                    Sidebar.sidebar_ "main-sidebar" perspectiveHeader

        withCollapsedContext s =
            case perspective of
                Perspective.Root _ ->
                    s

                Perspective.Namespace ns ->
                    Sidebar.withCollapsedContext
                        (viewNamespacePerspectiveHeader
                            cfg.changePerspectiveToNamespaceMsg
                            ns
                        )
                        s
    in
    sidebar
        |> withCollapsedContext
        |> Sidebar.withCollapsedActions [ Button.icon cfg.showFinderModalMsg Icon.browse |> Button.small ]


viewPerspectiveLandingPage :
    (ReadmeCard.Msg -> msg)
    -> msg
    -> Perspective
    -> ReadmeCard.Model
    -> List (Html msg)
viewPerspectiveLandingPage readmeCardMsg showFinderModalMsg perspective readmeCard =
    let
        ( details_, emptyState ) =
            case perspective of
                Perspective.Root { details } ->
                    ( details, EmptyState.view "Browse Code" (Click.onClick showFinderModalMsg) )

                Perspective.Namespace { fqn, details } ->
                    ( details, EmptyState.view (FQN.toString fqn) (Click.onClick showFinderModalMsg) )
    in
    case details_ of
        NotAsked ->
            [ viewLoading ]

        Loading ->
            [ viewLoading ]

        Success d ->
            [ viewReadme
                readmeCardMsg
                readmeCard
                (Namespace.readme d)
                emptyState
            ]

        Failure _ ->
            -- TODO: renable after API supports root namespace details [ viewError err ]
            [ EmptyState.view "Browse Code" (Click.onClick showFinderModalMsg) ]
