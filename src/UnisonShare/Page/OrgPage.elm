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
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Org as Org exposing (OrgSummary)
import UnisonShare.OrgPageHeader as OrgPageHeader
import UnisonShare.Page.OrgPeoplePage as OrgPeoplePage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route exposing (OrgRoute(..))



-- MODEL


type SubPage
    = People OrgPeoplePage.Model
    | Settings


type alias Model =
    { org : WebData OrgSummary
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
    = FetchOrgFinished (WebData OrgSummary)
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
    ShareApi.user handle
        |> HttpApi.toRequest Org.decodeSummary (RemoteData.fromResult >> FetchOrgFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewErrorPage : AppContext -> SubPage -> UserHandle -> Http.Error -> AppDocument msg
viewErrorPage _ subPage handle error =
    let
        page =
            case ( error, subPage ) of
                ( Http.BadStatus 404, _ ) ->
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

                _ ->
                    PageLayout.centeredLayout
                        (PageContent.oneColumn [ StatusMessage.bad "Error, could not load page" [] |> StatusMessage.view ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground
    in
    { pageId = "org-page org-page-error"
    , title = UserHandle.toString handle ++ " | Error"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Just OrgPageHeader.error
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
    let
        orgPageHeader activeNavItem org =
            OrgPageHeader.view
                ToggleMobileNav
                model.mobileNavIsOpen
                activeNavItem
                handle
                org

        appDoc activeNavItem org page modal =
            let
                pageId =
                    case activeNavItem of
                        OrgPageHeader.OrgProfile ->
                            "org-page org-profile-page"

                        OrgPageHeader.People ->
                            "org-page org-people-page"

                        OrgPageHeader.Settings ->
                            "org-page org-settings-page"
            in
            { pageId = pageId
            , title = UserHandle.toString handle ++ " | Loading..."
            , appHeader = AppHeader.appHeader AppHeader.None
            , pageHeader = Just (orgPageHeader activeNavItem org)
            , page = page
            , modal = modal
            }
    in
    case model.org of
        NotAsked ->
            viewLoadingPage appContext model.subPage handle

        Loading ->
            viewLoadingPage appContext model.subPage handle

        Failure e ->
            viewErrorPage appContext model.subPage handle e

        Success org ->
            case model.subPage of
                People people ->
                    let
                        ( page, modal ) =
                            OrgPeoplePage.view people
                    in
                    appDoc
                        OrgPageHeader.People
                        org
                        (page |> PageLayout.map OrgPeoplePageMsg |> PageLayout.view)
                        (Maybe.map (Html.map OrgPeoplePageMsg) modal)

                Settings ->
                    appDoc OrgPageHeader.Settings
                        org
                        (PageLayout.centeredNarrowLayout (PageContent.oneColumn [ text "Settings" ])
                            PageFooter.pageFooter
                            |> PageLayout.view
                        )
                        Nothing
