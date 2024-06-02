module UnisonShare.Page.UserPage exposing (..)

import Html exposing (h2, text)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import RemoteData exposing (RemoteData(..), WebData)
import Tuple
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.StatusMessage as StatusMessage
import UI.ViewMode exposing (ViewMode)
import UnisonShare.Account exposing (AccountSummary)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.CodeBrowsingContext as CodeBrowsingContext
import UnisonShare.CodebaseStatus as CodebaseStatus exposing (CodebaseStatus)
import UnisonShare.Page.CodePage as CodePage
import UnisonShare.Page.UserContributionsPage as UserContributionsPage
import UnisonShare.Page.UserProfilePage as UserProfilePage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route as Route exposing (CodeRoute, UserRoute(..))
import UnisonShare.Session as Session
import UnisonShare.SetupInstructions as SetupInstructions
import UnisonShare.User as User exposing (UserDetails)
import UnisonShare.UserPageHeader as UserPageHeader



-- MODEL


type SubPage
    = Profile UserProfilePage.Model
    | Code ViewMode CodePage.Model
    | Contributions UserContributionsPage.Model


type alias Model =
    { user : WebData UserDetails
    , subPage : SubPage
    , codebaseStatus : WebData CodebaseStatus
    , setupInstructions : Maybe ( SetupInstructions.Model, AccountSummary )
    , mobileNavIsOpen : Bool
    }


init : AppContext -> UserHandle -> UserRoute -> ( Model, Cmd Msg )
init appContext handle userRoute =
    let
        codeBrowsingContext =
            CodeBrowsingContext.UserCode handle

        ( subPage, cmd ) =
            case userRoute of
                UserProfile ->
                    let
                        ( profilePage, profileCmd ) =
                            UserProfilePage.init appContext handle
                    in
                    ( Profile profilePage, Cmd.map UserProfilePageMsg profileCmd )

                UserCode vm codeRoute ->
                    let
                        ( codePage, codePageCmd ) =
                            CodePage.init appContext codeBrowsingContext codeRoute
                    in
                    ( Code vm codePage, Cmd.map CodePageMsg codePageCmd )

                UserContributions ->
                    let
                        ( contributionsPage, contributionsCmd ) =
                            UserContributionsPage.init appContext handle
                    in
                    ( Contributions contributionsPage, Cmd.map UserContributionsPageMsg contributionsCmd )

        setupInstructions =
            case appContext.session of
                Session.SignedIn a ->
                    if UserHandle.equals handle a.handle then
                        Just ( SetupInstructions.init appContext a, a )

                    else
                        Nothing

                _ ->
                    Nothing
    in
    ( { user = Loading
      , codebaseStatus = Loading
      , subPage = subPage
      , setupInstructions = Maybe.map (\( ( c, _ ), a ) -> ( c, a )) setupInstructions
      , mobileNavIsOpen = False
      }
    , Cmd.batch
        [ fetchUser appContext handle
        , CodebaseStatus.checkStatus CodebaseStatusCheckFinished appContext codeBrowsingContext
        , cmd
        , setupInstructions
            |> Maybe.map (Tuple.first >> Tuple.second)
            |> Maybe.map (Cmd.map SetupInstructionsMsg)
            |> Maybe.withDefault Cmd.none
        ]
    )



-- UPDATE


type Msg
    = FetchUserFinished (WebData UserDetails)
    | CodebaseStatusCheckFinished (HttpResult CodebaseStatus)
    | ToggleMobileNav
    | SetupInstructionsMsg SetupInstructions.Msg
    | UserProfilePageMsg UserProfilePage.Msg
    | CodePageMsg CodePage.Msg
    | UserContributionsPageMsg UserContributionsPage.Msg


update : AppContext -> UserHandle -> UserRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext handle route msg model =
    case ( model.subPage, msg ) of
        ( _, FetchUserFinished u ) ->
            ( { model | user = u }, Cmd.none )

        ( _, CodebaseStatusCheckFinished r ) ->
            ( { model | codebaseStatus = RemoteData.fromResult r }, Cmd.none )

        ( Code viewMode _, SetupInstructionsMsg csiMsg ) ->
            case ( model.setupInstructions, route ) of
                ( Just ( csi, a ), Route.UserCode _ cr ) ->
                    let
                        ( csi_, csiCmd, csiOut ) =
                            SetupInstructions.update appContext a csiMsg csi

                        ( newModel, outCmd ) =
                            case csiOut of
                                SetupInstructions.Remain ->
                                    ( { model | setupInstructions = Just ( csi_, a ) }, Cmd.none )

                                SetupInstructions.NoLongerEmpty ->
                                    let
                                        ( codePage, codePageCmd ) =
                                            CodePage.init appContext (CodeBrowsingContext.UserCode handle) cr
                                    in
                                    ( { model
                                        | setupInstructions = Just ( csi_, a )
                                        , codebaseStatus = Success CodebaseStatus.NotEmpty
                                        , subPage = Code viewMode codePage
                                      }
                                    , Cmd.map CodePageMsg codePageCmd
                                    )

                                SetupInstructions.Exit ->
                                    ( { model | setupInstructions = Nothing }, Cmd.none )
                    in
                    ( newModel, Cmd.batch [ Cmd.map SetupInstructionsMsg csiCmd, outCmd ] )

                _ ->
                    ( model, Cmd.none )

        ( _, ToggleMobileNav ) ->
            ( { model | mobileNavIsOpen = not model.mobileNavIsOpen }, Cmd.none )

        -- Sub msgs
        ( Profile profilePage, UserProfilePageMsg userProfilePageMsg ) ->
            let
                ( profilePage_, profileCmd, out ) =
                    UserProfilePage.update appContext handle model.user userProfilePageMsg profilePage

                user =
                    case out of
                        UserProfilePage.NoOut ->
                            model.user

                        UserProfilePage.UpdateUserProfile u ->
                            RemoteData.map (\_ -> u) model.user
            in
            ( { model | subPage = Profile profilePage_, user = user }, Cmd.map UserProfilePageMsg profileCmd )

        ( Code viewMode codePage, CodePageMsg codePageMsg ) ->
            let
                codeBrowsingContext =
                    CodeBrowsingContext.UserCode handle
            in
            case route of
                Route.UserCode _ cr ->
                    let
                        ( codePage_, codePageCmd ) =
                            CodePage.update appContext codeBrowsingContext viewMode cr codePageMsg codePage
                    in
                    ( { model | subPage = Code viewMode codePage_ }
                    , Cmd.map CodePageMsg codePageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Contributions contributionsPage, UserContributionsPageMsg userContributionsPageMsg ) ->
            let
                ( contributionsPage_, contributionsCmd ) =
                    UserContributionsPage.update appContext handle userContributionsPageMsg contributionsPage
            in
            ( { model | subPage = Contributions contributionsPage_ }, Cmd.map UserContributionsPageMsg contributionsCmd )

        _ ->
            ( model, Cmd.none )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : AppContext -> UserHandle -> Model -> UserRoute -> ( Model, Cmd Msg )
updateSubPage appContext handle model route =
    let
        codeBrowsingContext =
            CodeBrowsingContext.UserCode handle
    in
    case route of
        UserProfile ->
            case model.subPage of
                Profile _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( profilePage, profileCmd ) =
                            UserProfilePage.init appContext handle
                    in
                    ( { model | subPage = Profile profilePage }, Cmd.map UserProfilePageMsg profileCmd )

        UserCode vm codeRoute ->
            case model.subPage of
                Code _ codeSubPage ->
                    let
                        ( codePage, codePageCmd ) =
                            CodePage.updateSubPage appContext codeBrowsingContext codeRoute codeSubPage
                    in
                    ( { model | subPage = Code vm codePage }
                    , Cmd.map CodePageMsg codePageCmd
                    )

                _ ->
                    let
                        ( codePage, codePageCmd ) =
                            CodePage.init appContext codeBrowsingContext codeRoute
                    in
                    ( { model | subPage = Code vm codePage }, Cmd.map CodePageMsg codePageCmd )

        UserContributions ->
            case model.subPage of
                Contributions _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( contributionsPage, contributionsCmd ) =
                            UserContributionsPage.init appContext handle
                    in
                    ( { model | subPage = Contributions contributionsPage }, Cmd.map UserContributionsPageMsg contributionsCmd )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.subPage of
        Code _ ucp ->
            Sub.map CodePageMsg (CodePage.subscriptions ucp)

        _ ->
            Sub.none



-- EFFECTS


navigateToCode : AppContext -> UserHandle -> CodeRoute -> Cmd Msg
navigateToCode appContext handle codeRoute =
    Route.navigate appContext.navKey (Route.userCode handle codeRoute)


fetchUser : AppContext -> UserHandle -> Cmd Msg
fetchUser appContext handle =
    ShareApi.user handle
        |> HttpApi.toRequest User.decodeDetails (RemoteData.fromResult >> FetchUserFinished)
        |> HttpApi.perform appContext.api



-- HELPERS


isSignedInUser : AppContext -> Model -> Bool
isSignedInUser appContext model =
    case ( appContext.session, model.user ) of
        ( Session.SignedIn account, Success user ) ->
            UserHandle.equals account.handle user.handle

        _ ->
            False



-- VIEW


viewErrorPage : AppContext -> SubPage -> UserHandle -> Http.Error -> AppDocument msg
viewErrorPage appContext subPage handle error =
    let
        page =
            case ( error, subPage ) of
                ( Http.BadStatus 404, Code _ _ ) ->
                    PageLayout.sidebarEdgeToEdgeLayout
                        appContext.operatingSystem
                        (Sidebar.empty "main-sidebar")
                        (PageContent.oneColumn [ StatusMessage.bad "Error, page not found" [] |> StatusMessage.view ])
                        PageFooter.pageFooter

                ( Http.BadStatus 404, _ ) ->
                    PageLayout.centeredLayout
                        (PageContent.oneColumn
                            [ EmptyState.iconCloud
                                (EmptyState.IconCenterPiece Icon.profile)
                                |> EmptyState.withContent [ h2 [] [ text ("Couldn't find user " ++ UserHandle.toString handle) ] ]
                                |> EmptyStateCard.view
                            ]
                        )
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                ( _, Code _ _ ) ->
                    PageLayout.sidebarEdgeToEdgeLayout
                        appContext.operatingSystem
                        (Sidebar.empty "main-sidebar")
                        (PageContent.oneColumn [ StatusMessage.bad "Error, could not load page" [] |> StatusMessage.view ])
                        PageFooter.pageFooter

                _ ->
                    PageLayout.centeredLayout
                        (PageContent.oneColumn [ StatusMessage.bad "Error, could not load page" [] |> StatusMessage.view ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground
    in
    { pageId = "user-page user-page-error"
    , title = UserHandle.toString handle ++ " | Error"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Just UserPageHeader.error
    , page = PageLayout.view page
    , modal = Nothing
    }


viewLoadingPage : AppContext -> SubPage -> UserHandle -> AppDocument msg
viewLoadingPage appContext subPage handle =
    let
        ( page, pageId ) =
            case subPage of
                Profile _ ->
                    ( UserProfilePage.viewLoadingPage, "user-profile-page" )

                Code _ _ ->
                    ( PageLayout.sidebarLeftContentLayout
                        appContext.operatingSystem
                        (Sidebar.empty "main-sidebar")
                        (PageContent.oneColumn [ text "" ])
                        PageFooter.pageFooter
                    , "code-page"
                    )

                Contributions _ ->
                    ( UserContributionsPage.viewLoadingPage, "user-contributions-page" )
    in
    { pageId = "user-page user-page_loading " ++ pageId
    , title = UserHandle.toString handle ++ " | Loading..."
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Just UserPageHeader.loading
    , page = PageLayout.view page
    , modal = Nothing
    }


view : AppContext -> UserHandle -> Model -> AppDocument Msg
view appContext handle model =
    let
        handle_ =
            UserHandle.toString handle

        userProfilePageHeader activeNavItem user =
            UserPageHeader.view
                ToggleMobileNav
                model.mobileNavIsOpen
                activeNavItem
                handle
                user
    in
    case model.user of
        NotAsked ->
            viewLoadingPage appContext model.subPage handle

        Loading ->
            viewLoadingPage appContext model.subPage handle

        Failure e ->
            viewErrorPage appContext model.subPage handle e

        Success user ->
            case model.subPage of
                Profile profilePage ->
                    let
                        ( page, modal ) =
                            UserProfilePage.view appContext.session user profilePage
                                |> Tuple.mapFirst (PageLayout.map UserProfilePageMsg)
                                |> Tuple.mapFirst PageLayout.view
                                |> Tuple.mapSecond (Maybe.map (Html.map UserProfilePageMsg))

                        activeNavItem =
                            if isSignedInUser appContext model then
                                AppHeader.Profile

                            else
                                AppHeader.None
                    in
                    { pageId = "user-page user-profile-page"
                    , title = handle_
                    , appHeader = AppHeader.appHeader activeNavItem
                    , pageHeader = Just (userProfilePageHeader UserPageHeader.UserProfile user)
                    , page = page
                    , modal = modal
                    }

                Contributions contributionsPage ->
                    { pageId = "user-page user-contributions-page"
                    , title = handle_ ++ " | Contributions"
                    , appHeader = AppHeader.appHeader AppHeader.None
                    , pageHeader = Just (userProfilePageHeader UserPageHeader.Contributions user)
                    , page =
                        UserContributionsPage.view handle contributionsPage
                            |> PageLayout.map UserContributionsPageMsg
                            |> PageLayout.view
                    , modal = Nothing
                    }

                Code viewMode_ codeSubPage ->
                    let
                        pageTitle =
                            handle_ ++ " | Code"

                        appDoc page viewMode modal =
                            { pageId = "user-page code-page"
                            , title = pageTitle
                            , appHeader =
                                AppHeader.appHeader AppHeader.None
                                    |> AppHeader.withViewMode viewMode
                            , pageHeader = Just (userProfilePageHeader UserPageHeader.Code user)
                            , page = PageLayout.view page
                            , modal = modal
                            }
                    in
                    case model.codebaseStatus of
                        Success CodebaseStatus.Empty ->
                            let
                                ( codePage, modal_ ) =
                                    CodePage.view appContext
                                        CodePageMsg
                                        viewMode_
                                        CodebaseStatus.Empty
                                        codeSubPage
                            in
                            case model.setupInstructions of
                                Just ( csi, a ) ->
                                    appDoc
                                        (codePage
                                            |> PageLayout.withContent
                                                (PageContent.oneColumn [ Html.map SetupInstructionsMsg (SetupInstructions.view appContext a csi) ])
                                        )
                                        viewMode_
                                        modal_

                                Nothing ->
                                    appDoc codePage viewMode_ modal_

                        Success CodebaseStatus.NotEmpty ->
                            let
                                ( codePage, modal_ ) =
                                    CodePage.view appContext
                                        CodePageMsg
                                        viewMode_
                                        CodebaseStatus.NotEmpty
                                        codeSubPage
                            in
                            appDoc codePage viewMode_ modal_

                        Failure e ->
                            viewErrorPage appContext model.subPage handle e

                        _ ->
                            viewLoadingPage appContext model.subPage handle
