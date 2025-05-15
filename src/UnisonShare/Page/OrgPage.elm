module UnisonShare.Page.OrgPage exposing (..)

import Html exposing (h2, text)
import Http
import Lib.HttpApi as HttpApi
import Lib.UserHandle as UserHandle exposing (UserHandle)
import RemoteData exposing (RemoteData(..), WebData)
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Org as Org exposing (OrgDetails)
import UnisonShare.OrgPageHeader as OrgPageHeader
import UnisonShare.Page.ErrorPage as ErrorPage
import UnisonShare.Page.OrgPeoplePage as OrgPeoplePage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route exposing (OrgRoute(..))



-- MODEL


type SubPage
    = People OrgPeoplePage.Model
    | Settings


type alias Model =
    { org : WebData OrgDetails
    , subPage : SubPage
    , mobileNavIsOpen : Bool
    }


init : AppContext -> UserHandle -> OrgRoute -> ( Model, Cmd Msg )
init appContext orgHandle route =
    let
        ( subPage, cmd ) =
            case route of
                OrgPeople ->
                    let
                        ( p, cmd_ ) =
                            OrgPeoplePage.init appContext orgHandle
                    in
                    ( People p, Cmd.map OrgPeoplePageMsg cmd_ )

                OrgSettings ->
                    ( Settings, Cmd.none )
    in
    ( { org = Loading
      , subPage = subPage
      , mobileNavIsOpen = False
      }
    , Cmd.batch [ fetchOrg appContext orgHandle, cmd ]
    )



-- UPDATE


type Msg
    = FetchOrgFinished (WebData OrgDetails)
    | ToggleMobileNav
    | OrgPeoplePageMsg OrgPeoplePage.Msg


update : AppContext -> UserHandle -> OrgRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext orgHandle _ msg model =
    case ( model.subPage, msg ) of
        ( _, FetchOrgFinished o ) ->
            ( { model | org = o }, Cmd.none )

        ( _, ToggleMobileNav ) ->
            ( { model | mobileNavIsOpen = not model.mobileNavIsOpen }, Cmd.none )

        ( People people, OrgPeoplePageMsg pMsg ) ->
            let
                ( people_, pCmd ) =
                    OrgPeoplePage.update appContext orgHandle pMsg people
            in
            ( { model | subPage = People people_ }, Cmd.map OrgPeoplePageMsg pCmd )

        _ ->
            ( model, Cmd.none )


updateSubPage : AppContext -> UserHandle -> Model -> OrgRoute -> ( Model, Cmd Msg )
updateSubPage _ _ model _ =
    ( model, Cmd.none )



-- EFFECTS


fetchOrg : AppContext -> UserHandle -> Cmd Msg
fetchOrg appContext handle =
    ShareApi.org handle
        |> HttpApi.toRequest Org.decodeDetails (RemoteData.fromResult >> FetchOrgFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewErrorPage : AppContext -> UserHandle -> Http.Error -> AppDocument msg
viewErrorPage appContext handle error =
    let
        page =
            case error of
                Http.BadStatus 404 ->
                    PageLayout.centeredLayout
                        (PageContent.oneColumn
                            [ EmptyState.iconCloud
                                (EmptyState.IconCenterPiece Icon.profile)
                                |> EmptyState.withContent [ h2 [] [ text ("Couldn't find organization " ++ UserHandle.toString handle) ] ]
                                |> EmptyStateCard.view
                            ]
                        )
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Http.BadStatus 403 ->
                    PageLayout.centeredLayout
                        (PageContent.oneColumn
                            [ EmptyState.iconCloud
                                (EmptyState.IconCenterPiece Icon.profile)
                                |> EmptyState.withContent [ h2 [] [ text "You're not authorized to view this page" ] ]
                                |> EmptyStateCard.view
                            ]
                        )
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                _ ->
                    ErrorPage.view appContext.session error "organization" "organization-error"
    in
    { pageId = "org-page org-page-error"
    , title = UserHandle.toString handle ++ " | Error"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Just (OrgPageHeader.error handle)
    , page = PageLayout.view page
    , modal = Nothing
    }


viewLoadingPage : AppContext -> SubPage -> UserHandle -> AppDocument msg
viewLoadingPage _ _ handle =
    { pageId = "org-page org-page_loading "
    , title = UserHandle.toString handle ++ " | Loading..."
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Just OrgPageHeader.loading
    , page =
        PageLayout.centeredNarrowLayout (PageContent.oneColumn [ text "" ])
            PageFooter.pageFooter
            |> PageLayout.view
    , modal = Nothing
    }


view : AppContext -> UserHandle -> Model -> AppDocument Msg
view appContext handle model =
    case model.org of
        NotAsked ->
            viewLoadingPage appContext model.subPage handle

        Loading ->
            viewLoadingPage appContext model.subPage handle

        Failure e ->
            viewErrorPage appContext handle e

        Success org ->
            if Org.canManage org then
                let
                    orgPageHeader activeNavItem =
                        OrgPageHeader.view
                            ToggleMobileNav
                            model.mobileNavIsOpen
                            activeNavItem
                            handle
                            org

                    appDoc activeNavItem page modal =
                        let
                            ( pageId, pageTitle ) =
                                case activeNavItem of
                                    OrgPageHeader.OrgProfile ->
                                        ( "org-page org-profile-page", "Profile" )

                                    OrgPageHeader.People ->
                                        ( "org-page org-people-page", "People" )

                                    OrgPageHeader.Settings ->
                                        ( "org-page org-settings-page", "Settings" )
                        in
                        { pageId = pageId
                        , title = UserHandle.toString handle ++ " | " ++ pageTitle
                        , appHeader = AppHeader.appHeader AppHeader.None
                        , pageHeader = Just (orgPageHeader activeNavItem)
                        , page = page
                        , modal = modal
                        }
                in
                case model.subPage of
                    People people ->
                        let
                            ( page, modal ) =
                                OrgPeoplePage.view people
                        in
                        appDoc
                            OrgPageHeader.People
                            (page |> PageLayout.map OrgPeoplePageMsg |> PageLayout.view)
                            (Maybe.map (Html.map OrgPeoplePageMsg) modal)

                    Settings ->
                        appDoc OrgPageHeader.Settings
                            (PageLayout.centeredNarrowLayout (PageContent.oneColumn [ text "TODO: Settings" ])
                                PageFooter.pageFooter
                                |> PageLayout.view
                            )
                            Nothing

            else
                viewErrorPage appContext handle (Http.BadStatus 403)
