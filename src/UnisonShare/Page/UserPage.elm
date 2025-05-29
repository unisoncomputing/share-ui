module UnisonShare.Page.UserPage exposing (..)

import Html exposing (div, h1, h2, p, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import Lib.UserHandle as UserHandle exposing (UserHandle)
import RemoteData exposing (RemoteData(..), WebData)
import UI.CopyField as CopyField
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Page.CodePage as CodePage
import UnisonShare.Page.UserContributionsPage as UserContributionsPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route exposing (UserRoute(..))
import UnisonShare.Session as Session
import UnisonShare.User as User exposing (UserDetails)
import UnisonShare.UserPageHeader as UserPageHeader



-- MODEL


type SubPage
    = Code
    | Contributions UserContributionsPage.Model


type alias Model =
    { user : WebData UserDetails
    , subPage : SubPage
    , mobileNavIsOpen : Bool
    }


init : AppContext -> UserHandle -> UserRoute -> ( Model, Cmd Msg )
init appContext handle userRoute =
    let
        ( subPage, cmd ) =
            case userRoute of
                UserCode ->
                    ( Code, Cmd.none )

                UserContributions ->
                    let
                        ( contributionsPage, contributionsCmd ) =
                            UserContributionsPage.init appContext handle
                    in
                    ( Contributions contributionsPage, Cmd.map UserContributionsPageMsg contributionsCmd )
    in
    ( { user = Loading
      , subPage = subPage
      , mobileNavIsOpen = False
      }
    , Cmd.batch
        [ fetchUser appContext handle
        , cmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchUserFinished (WebData UserDetails)
    | ToggleMobileNav
    | CodePageMsg CodePage.Msg
    | UserContributionsPageMsg UserContributionsPage.Msg


update : AppContext -> UserHandle -> UserRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext handle _ msg model =
    case ( model.subPage, msg ) of
        ( _, FetchUserFinished u ) ->
            ( { model | user = u }, Cmd.none )

        ( _, ToggleMobileNav ) ->
            ( { model | mobileNavIsOpen = not model.mobileNavIsOpen }, Cmd.none )

        -- Sub msgs
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
    case route of
        UserCode ->
            ( { model | subPage = Code }, Cmd.none )

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



-- EFFECTS


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
                ( Http.BadStatus 404, Code ) ->
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

                ( _, Code ) ->
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
    , appHeader = AppHeader.appHeader
    , pageHeader = Just UserPageHeader.error
    , page = PageLayout.view page
    , modal = Nothing
    }


viewLoadingPage : AppContext -> SubPage -> UserHandle -> AppDocument msg
viewLoadingPage appContext subPage handle =
    let
        ( page, pageId ) =
            case subPage of
                Code ->
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
    , appHeader = AppHeader.appHeader
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
                appContext.session
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
                Contributions contributionsPage ->
                    { pageId = "user-page user-contributions-page"
                    , title = handle_ ++ " | Contributions"
                    , appHeader = AppHeader.appHeader
                    , pageHeader = Just (userProfilePageHeader UserPageHeader.Contributions user)
                    , page =
                        UserContributionsPage.view handle contributionsPage
                            |> PageLayout.map UserContributionsPageMsg
                            |> PageLayout.view
                    , modal = Nothing
                    }

                Code ->
                    let
                        pageTitle =
                            handle_ ++ " | Code"
                    in
                    { pageId = "user-page code-page"
                    , title = pageTitle
                    , appHeader = AppHeader.appHeader
                    , pageHeader = Just (userProfilePageHeader UserPageHeader.Code user)
                    , page =
                        PageLayout.centeredNarrowLayout
                            (PageContent.oneColumn
                                [ h1 [] [ text "Non-project Code" ]
                                , EmptyState.iconCloud
                                    (EmptyState.IconCenterPiece Icon.documentCode)
                                    |> EmptyState.withContent
                                        [ h2 [] [ text "Code outside of projects are deprecated" ]
                                        , p []
                                            [ text "If you have any on Unison Share, you can pull it into a new project: " ]
                                        , div [ class "pull-instructions" ]
                                            [ CopyField.copyField (\_ -> NoOp) "project.create-empty backup-public"
                                                |> CopyField.withPrefix "scratch/main>"
                                                |> CopyField.view
                                            , CopyField.copyField (\_ -> NoOp) ("pull " ++ UserHandle.toUnprefixedString user.handle ++ ".public")
                                                |> CopyField.withPrefix "backup-project/main>"
                                                |> CopyField.view
                                            ]
                                        ]
                                    |> EmptyStateCard.view
                                ]
                            )
                            PageFooter.pageFooter
                            |> PageLayout.withSubduedBackground
                            |> PageLayout.view
                    , modal = Nothing
                    }
