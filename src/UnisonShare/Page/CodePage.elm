module UnisonShare.Page.CodePage exposing (..)

import Code.CodebaseTree as CodebaseTree
import Code.Config exposing (Config)
import Code.Definition.Reference exposing (Reference)
import Code.Finder as Finder
import Code.Finder.SearchOptions as SearchOptions
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.FullyQualifiedNameSet as FQNSet
import Code.Namespace exposing (NamespaceDetails)
import Code.Perspective as Perspective exposing (Perspective)
import Code.ReadmeCard as ReadmeCard
import Code2.Workspace.WorkspaceItemRef as WorkspaceItemRef
import Code2.Workspace.WorkspacePanes as WorkspacePanes
import Html exposing (Html)
import Lib.HttpApi as HttpApi
import RemoteData exposing (WebData)
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.Sidebar as Sidebar exposing (Sidebar)
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.CodeBrowsingContext exposing (CodeBrowsingContext(..))
import UnisonShare.Page.CodePageContent as CodePageContent
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route as Route exposing (CodeRoute(..))



-- MODEL


type PageModal
    = NoModal
    | FinderModal Finder.Model


type CodeContent
    = PerspectivePage ReadmeCard.Model
    | WorkspacePage WorkspacePanes.Model


type alias Model =
    { content : CodeContent
    , sidebarToggled : Bool
    , codebaseTree : CodebaseTree.Model
    , config : Config
    , modal : PageModal
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> CodeBrowsingContext -> CodeRoute -> ( Model, Cmd Msg )
init appContext context codeRoute =
    let
        config =
            AppContext.toCodeConfig appContext context perspective

        perspective =
            case codeRoute of
                CodeRoot p ->
                    Perspective.fromParams p

                Definition p _ ->
                    Perspective.fromParams p

                DependentsOf p _ ->
                    Perspective.fromParams p

        ( codebaseTree, codebaseTreeCmd ) =
            CodebaseTree.init config

        ( content, cmd ) =
            case codeRoute of
                CodeRoot _ ->
                    ( PerspectivePage ReadmeCard.init
                    , Cmd.none
                    )

                Definition _ ref ->
                    let
                        ( workspaceInit, workspaceCmdInit ) =
                            WorkspacePanes.init
                                appContext.operatingSystem

                        ( workspace, workspaceCmd ) =
                            WorkspacePanes.openDefinition config workspaceInit ref
                    in
                    ( WorkspacePage workspace
                    , Cmd.batch
                        [ Cmd.map WorkspacePanesMsg workspaceCmdInit
                        , Cmd.map WorkspacePanesMsg workspaceCmd
                        ]
                    )

                DependentsOf _ ref ->
                    let
                        ( workspaceInit, workspaceCmdInit ) =
                            WorkspacePanes.init
                                appContext.operatingSystem

                        ( workspace, workspaceCmd ) =
                            WorkspacePanes.openDependentsOf config workspaceInit ref
                    in
                    ( WorkspacePage workspace
                    , Cmd.batch
                        [ Cmd.map WorkspacePanesMsg workspaceCmdInit
                        , Cmd.map WorkspacePanesMsg workspaceCmd
                        ]
                    )

        fetchNamespaceDetailsCmd =
            config.perspective
                |> CodePageContent.fetchNamespaceDetails
                    FetchPerspectiveNamespaceDetailsFinished
                    context
                |> Maybe.map (HttpApi.perform appContext.api)
                |> Maybe.withDefault Cmd.none
    in
    ( { content = content
      , sidebarToggled = False
      , codebaseTree = codebaseTree
      , config = config
      , modal = NoModal
      , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
      }
    , Cmd.batch
        [ cmd
        , Cmd.map CodebaseTreeMsg codebaseTreeCmd
        , fetchNamespaceDetailsCmd
        ]
    )



-- UPDATE


type Msg
    = ShowFinderModal
    | CloseModal
    | UpOneLevel
    | ChangePerspectiveToNamespace FQN
    | ToggleSidebar
    | FetchPerspectiveNamespaceDetailsFinished FQN (WebData NamespaceDetails)
    | ReadmeCardMsg ReadmeCard.Msg
    | Keydown KeyboardEvent
    | FinderMsg Finder.Msg
    | KeyboardShortcutMsg KeyboardShortcut.Msg
    | CodebaseTreeMsg CodebaseTree.Msg
    | WorkspacePanesMsg WorkspacePanes.Msg


update : AppContext -> CodeBrowsingContext -> CodeRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext context codeRoute msg model =
    case ( model.content, msg ) of
        ( _, ShowFinderModal ) ->
            let
                ( fm, fCmd ) =
                    Finder.init model.config (SearchOptions.init model.config.perspective Nothing)
            in
            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fCmd )

        ( _, CloseModal ) ->
            ( { model | modal = NoModal }, Cmd.none )

        ( _, FetchPerspectiveNamespaceDetailsFinished fqn details ) ->
            let
                config =
                    model.config

                perspective =
                    case model.config.perspective of
                        Perspective.Root r ->
                            if FQN.isRoot fqn then
                                Perspective.Root { r | details = details }

                            else
                                config.perspective

                        Perspective.Namespace p ->
                            if FQN.equals p.fqn fqn then
                                Perspective.Namespace { p | details = details }

                            else
                                config.perspective

                nextConfig =
                    { config | perspective = perspective }
            in
            ( { model | config = nextConfig }, Cmd.none )

        ( _, UpOneLevel ) ->
            let
                newPerspective =
                    Perspective.upOneLevel model.config.perspective

                navCmd =
                    navigateToCode appContext context (Route.replacePerspective (routeReference codeRoute) newPerspective)
            in
            ( model, navCmd )

        ( _, ChangePerspectiveToNamespace fqn ) ->
            let
                perspective =
                    Perspective.toNamespacePerspective model.config.perspective fqn

                navCmd =
                    navigateToCode appContext context (Route.replacePerspective (routeReference codeRoute) perspective)
            in
            ( model, navCmd )

        ( _, ToggleSidebar ) ->
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )

        ( _, CodebaseTreeMsg codebaseTreeMsg ) ->
            let
                ( codebaseTree, codebaseTreeCmd, outMsg ) =
                    CodebaseTree.update model.config codebaseTreeMsg model.codebaseTree

                ( m, cmd_ ) =
                    ( { model | codebaseTree = codebaseTree }
                    , Cmd.map CodebaseTreeMsg codebaseTreeCmd
                    )
            in
            case outMsg of
                CodebaseTree.None ->
                    ( m, cmd_ )

                CodebaseTree.OpenDefinition ref ->
                    let
                        navCmd =
                            navigateToCode appContext context (Route.definition model.config.perspective ref)

                        -- Close the sidebar when opening items on mobile
                        m_ =
                            if m.sidebarToggled then
                                { m | sidebarToggled = False }

                            else
                                m
                    in
                    ( m_, Cmd.batch [ cmd_, navCmd ] )

                CodebaseTree.ChangePerspectiveToNamespace fqn ->
                    let
                        perspective =
                            Perspective.toNamespacePerspective model.config.perspective fqn

                        ref =
                            case codeRoute of
                                Definition _ r ->
                                    Just r

                                _ ->
                                    Nothing

                        navCmd =
                            navigateToCode appContext context (Route.replacePerspective ref perspective)
                    in
                    ( m, Cmd.batch [ cmd_, navCmd ] )

        ( _, FinderMsg finderMsg ) ->
            case model.modal of
                FinderModal fm ->
                    let
                        ( fm_, fCmd, outMsg ) =
                            Finder.update model.config finderMsg fm
                    in
                    case outMsg of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm_ }, Cmd.map FinderMsg fCmd )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.map FinderMsg fCmd )

                        Finder.OpenDefinition r ->
                            ( { model | modal = NoModal }
                            , Cmd.batch
                                [ Cmd.map FinderMsg fCmd
                                , navigateToCode appContext context (Route.definition model.config.perspective r)
                                ]
                            )

                _ ->
                    ( model, Cmd.none )

        ( _, Keydown event ) ->
            keydown appContext model event

        ( _, KeyboardShortcutMsg kMsg ) ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg kCmd )

        ( PerspectivePage rm, ReadmeCardMsg readmeCardMsg ) ->
            let
                ( readmeCard, rmCmd, out ) =
                    ReadmeCard.update model.config readmeCardMsg rm

                navCmd =
                    case out of
                        ReadmeCard.OpenDefinition r ->
                            navigateToCode appContext context (Route.definition model.config.perspective r)

                        _ ->
                            Cmd.none
            in
            ( { model | content = PerspectivePage readmeCard }
            , Cmd.batch [ navCmd, Cmd.map ReadmeCardMsg rmCmd ]
            )

        ( WorkspacePage workspace, WorkspacePanesMsg workspaceMsg ) ->
            let
                ( workspace_, workspaceCmd, outMsg ) =
                    WorkspacePanes.update model.config workspaceMsg workspace

                ( m, outCmd ) =
                    case outMsg of
                        WorkspacePanes.Focused ref ->
                            -- Needs to skip if already focused on?
                            case ref of
                                WorkspaceItemRef.DefinitionItemRef dRef ->
                                    let
                                        route =
                                            Route.definition model.config.perspective dRef
                                    in
                                    ( model, navigateToCode appContext context route )

                                WorkspaceItemRef.DependentsItemRef dRef ->
                                    ( model, navigateToCode appContext context (Route.dependentsOf model.config.perspective dRef) )

                                _ ->
                                    ( model, Cmd.none )

                        WorkspacePanes.Emptied ->
                            ( model, navigateToCode appContext context (Route.codeRoot model.config.perspective) )

                        WorkspacePanes.ChangePerspectiveToSubNamespace ref subFqn ->
                            let
                                perspective =
                                    let
                                        fullFqn =
                                            case model.config.perspective of
                                                Perspective.Namespace { fqn } ->
                                                    FQN.append fqn subFqn

                                                _ ->
                                                    subFqn
                                    in
                                    Perspective.toNamespacePerspective model.config.perspective fullFqn
                            in
                            ( model, navigateToCode appContext context (Route.replacePerspective (Just ref) perspective) )

                        WorkspacePanes.ShowFinderRequest adhocFqn ->
                            let
                                ( fm, fCmd ) =
                                    Finder.init model.config (SearchOptions.init model.config.perspective (Just adhocFqn))
                            in
                            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fCmd )

                        _ ->
                            ( model, Cmd.none )
            in
            ( { m | content = WorkspacePage workspace_ }
            , Cmd.batch
                [ outCmd
                , Cmd.map WorkspacePanesMsg workspaceCmd
                ]
            )

        _ ->
            ( model, Cmd.none )


updateSubPage : AppContext -> CodeBrowsingContext -> CodeRoute -> Model -> ( Model, Cmd Msg )
updateSubPage appContext codeBrowsingContext codeRoute model =
    let
        toConfig =
            AppContext.toCodeConfig appContext codeBrowsingContext

        refreshSidebar newConfig m =
            CodePageContent.fetchPerspectiveAndCodebaseTree
                appContext
                newConfig
                FetchPerspectiveNamespaceDetailsFinished
                CodebaseTreeMsg
                codeBrowsingContext
                model.config.perspective
                m

        -- Don't replace the perspective (it has namespace details state) when the page refreshes.
        config p =
            if Perspective.equals p model.config.perspective then
                model.config

            else
                toConfig p
    in
    case codeRoute of
        CodeRoot p ->
            let
                persp =
                    p |> Perspective.fromParams

                config_ =
                    config persp

                ( model2, cmd ) =
                    refreshSidebar config_ model

                content =
                    case model.content of
                        PerspectivePage rc ->
                            PerspectivePage rc

                        _ ->
                            PerspectivePage ReadmeCard.init
            in
            ( { model2 | config = config_, content = content }, cmd )

        Definition p ref ->
            let
                persp =
                    p |> Perspective.fromParams

                config_ =
                    config persp

                ( workspace, workspaceCmd ) =
                    case model.content of
                        WorkspacePage ws ->
                            WorkspacePanes.openDefinition config_ ws ref

                        _ ->
                            let
                                ( initWs, initCmd ) =
                                    WorkspacePanes.init appContext.operatingSystem

                                ( openDef, openDefCmd ) =
                                    WorkspacePanes.openDefinition config_ initWs ref
                            in
                            ( openDef, Cmd.batch [ initCmd, openDefCmd ] )

                model2 =
                    { model | config = config_, content = WorkspacePage workspace }

                ( model3, cmd ) =
                    refreshSidebar config_ model2
            in
            ( model3
            , Cmd.batch [ Cmd.map WorkspacePanesMsg workspaceCmd, cmd ]
            )

        DependentsOf p ref ->
            let
                persp =
                    p |> Perspective.fromParams

                config_ =
                    config persp

                ( workspace, workspaceCmd ) =
                    case model.content of
                        WorkspacePage ws ->
                            WorkspacePanes.openDependentsOf config_ ws ref

                        _ ->
                            let
                                ( initWs, initCmd ) =
                                    WorkspacePanes.init appContext.operatingSystem

                                ( openDef, openDefCmd ) =
                                    WorkspacePanes.openDefinition config_ initWs ref
                            in
                            ( openDef, Cmd.batch [ initCmd, openDefCmd ] )

                model2 =
                    { model | config = config_, content = WorkspacePage workspace }

                ( model3, cmd ) =
                    refreshSidebar config_ model2
            in
            ( model3
            , Cmd.batch [ Cmd.map WorkspacePanesMsg workspaceCmd, cmd ]
            )


routeReference : CodeRoute -> Maybe Reference
routeReference route =
    case route of
        Definition _ r ->
            Just r

        _ ->
            Nothing


keydown : AppContext -> Model -> KeyboardEvent -> ( Model, Cmd Msg )
keydown appContext model keyboardEvent =
    let
        shortcut =
            KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut keyboardEvent

        noOp =
            ( model, Cmd.none )

        toggleSidebar =
            ( { model | sidebarToggled = not model.sidebarToggled }, Cmd.none )
    in
    case shortcut of
        KeyboardShortcut.Chord Ctrl (B _) ->
            toggleSidebar

        KeyboardShortcut.Chord Meta (B _) ->
            toggleSidebar

        KeyboardShortcut.Sequence _ Escape ->
            ( { model | modal = NoModal }, Cmd.none )

        _ ->
            if Finder.isShowFinderKeyboardShortcut appContext.operatingSystem shortcut then
                let
                    ( finder, cmd ) =
                        Finder.init model.config
                            (SearchOptions.init model.config.perspective Nothing)
                in
                ( { model | modal = FinderModal finder }, Cmd.map FinderMsg cmd )

            else
                noOp



-- EFFECTS


navigateToCode : AppContext -> CodeBrowsingContext -> CodeRoute -> Cmd Msg
navigateToCode appContext context codeRoute =
    let
        route_ =
            case context of
                ProjectBranch ps bs ->
                    Route.projectBranch ps bs codeRoute
    in
    Route.navigate appContext.navKey route_



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.content of
        PerspectivePage _ ->
            KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown

        WorkspacePage ws ->
            Sub.batch
                [ KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown
                , Sub.map WorkspacePanesMsg (WorkspacePanes.subscriptions ws)
                ]



-- VIEW


viewContent : AppContext -> Perspective -> CodeContent -> PageContent Msg
viewContent appContext perspective content =
    case content of
        PerspectivePage readmeCard ->
            PageContent.oneColumn
                (CodePageContent.viewPerspectiveLandingPage
                    ReadmeCardMsg
                    ShowFinderModal
                    perspective
                    readmeCard
                )

        WorkspacePage workspace ->
            let
                cfg =
                    { operatingSystem = appContext.operatingSystem
                    , withDependents = True
                    , withDependencies = False
                    , withNamespaceDropdown = True
                    , withFocusedPaneIndicator = False
                    }
            in
            PageContent.oneColumn [ Html.map WorkspacePanesMsg (WorkspacePanes.view cfg workspace) ]


viewSidebar : Model -> Sidebar Msg
viewSidebar model =
    let
        codebaseTree =
            Just { codebaseTree = model.codebaseTree, codebaseTreeMsg = CodebaseTreeMsg }

        openDefinitions =
            case model.content of
                WorkspacePage workspace ->
                    workspace
                        |> WorkspacePanes.currentlyOpenFqns
                        |> FQNSet.fromList

                _ ->
                    FQNSet.empty
    in
    CodePageContent.viewSidebar
        model.config.perspective
        { upOneLevelMsg = UpOneLevel
        , showFinderModalMsg = ShowFinderModal
        , changePerspectiveToNamespaceMsg = ChangePerspectiveToNamespace
        }
        openDefinitions
        codebaseTree
        |> Sidebar.withToggle
            { isToggled = model.sidebarToggled, toggleMsg = ToggleSidebar }


view : AppContext -> (Msg -> msg) -> Model -> ( PageLayout msg, Maybe (Html msg) )
view appContext toMsg model =
    let
        content =
            PageContent.map toMsg
                (viewContent
                    appContext
                    model.config.perspective
                    model.content
                )

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                FinderModal fm ->
                    Just (Html.map toMsg (Html.map FinderMsg (Finder.view fm)))
    in
    case model.content of
        PerspectivePage _ ->
            ( PageLayout.sidebarLeftContentLayout
                appContext.operatingSystem
                (Sidebar.map toMsg (viewSidebar model))
                content
                PageFooter.pageFooter
                |> PageLayout.withSidebarToggle model.sidebarToggled
            , modal
            )

        WorkspacePage _ ->
            ( PageLayout.sidebarEdgeToEdgeLayout
                appContext.operatingSystem
                (Sidebar.map toMsg (viewSidebar model))
                content
                PageFooter.pageFooter
                |> PageLayout.withSidebarToggle model.sidebarToggled
                |> PageLayout.withSubduedBackground
            , modal
            )
