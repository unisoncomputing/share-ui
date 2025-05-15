-- Users and Orgs profiles share a URL space, this module conditionally render
-- one or the other.


module UnisonShare.Page.ProfilePage exposing (..)

import Html exposing (div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Org as Org exposing (OrgDetails)
import UnisonShare.OrgPageHeader as OrgPageHeader
import UnisonShare.Page.OrgProfilePage as OrgProfilePage
import UnisonShare.Page.UserProfilePage as UserProfilePage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.User as User exposing (UserDetails)
import UnisonShare.UserPageHeader as UserPageHeader



-- MODEL


type Profile
    = User UserDetails
    | Org OrgDetails


type Model
    = Loading UserHandle
    | UserProfile
        { user : UserDetails
        , page : UserProfilePage.Model
        , mobileNavIsOpen : Bool
        }
    | OrgProfile
        { org : OrgDetails
        , page : OrgProfilePage.Model
        , mobileNavIsOpen : Bool
        }
    | Failure UserHandle Http.Error


init : AppContext -> UserHandle -> ( Model, Cmd Msg )
init appContext handle =
    ( Loading handle, fetchProfile appContext handle )



-- UPDATE


type Msg
    = FetchProfileFinished (HttpResult Profile)
    | ToggleMobileNav
    | UserProfilePageMsg UserProfilePage.Msg
    | OrgProfilePageMsg OrgProfilePage.Msg


update : AppContext -> UserHandle -> Msg -> Model -> ( Model, Cmd Msg )
update appContext handle msg model =
    case msg of
        FetchProfileFinished (Ok (User u)) ->
            let
                ( page, pageCmd ) =
                    UserProfilePage.init appContext handle
            in
            ( UserProfile { user = u, page = page, mobileNavIsOpen = False }
            , Cmd.map UserProfilePageMsg pageCmd
            )

        FetchProfileFinished (Ok (Org o)) ->
            let
                ( page, pageCmd ) =
                    OrgProfilePage.init appContext handle
            in
            ( OrgProfile { org = o, page = page, mobileNavIsOpen = False }
            , Cmd.map OrgProfilePageMsg pageCmd
            )

        FetchProfileFinished (Err e) ->
            ( Failure handle e, Cmd.none )

        ToggleMobileNav ->
            case model of
                UserProfile p ->
                    ( UserProfile { p | mobileNavIsOpen = not p.mobileNavIsOpen }, Cmd.none )

                OrgProfile p ->
                    ( OrgProfile { p | mobileNavIsOpen = not p.mobileNavIsOpen }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        UserProfilePageMsg userProfilePageMsg ->
            case model of
                UserProfile p ->
                    let
                        ( profilePage_, profileCmd, out ) =
                            UserProfilePage.update appContext handle p.user userProfilePageMsg p.page

                        user =
                            case out of
                                UserProfilePage.NoOut ->
                                    p.user

                                UserProfilePage.UpdateUserProfile u ->
                                    u
                    in
                    ( UserProfile { p | page = profilePage_, user = user }, Cmd.map UserProfilePageMsg profileCmd )

                _ ->
                    ( model, Cmd.none )

        OrgProfilePageMsg orgProfilePageMsg ->
            case model of
                OrgProfile p ->
                    let
                        ( profilePage_, profileCmd ) =
                            OrgProfilePage.update appContext handle p.org orgProfilePageMsg p.page
                    in
                    ( OrgProfile { p | page = profilePage_ }, Cmd.map OrgProfilePageMsg profileCmd )

                _ ->
                    ( model, Cmd.none )



-- EFFECTS


fetchProfile : AppContext -> UserHandle -> Cmd Msg
fetchProfile appContext handle =
    let
        decode =
            Decode.oneOf
                [ when (Decode.field "kind" Decode.string) ((==) "org") (Decode.map Org Org.decodeDetails)
                , when (Decode.field "kind" Decode.string) ((==) "user") (Decode.map User User.decodeDetails)
                ]
    in
    ShareApi.user handle
        |> HttpApi.toRequest decode FetchProfileFinished
        |> HttpApi.perform appContext.api


viewLoadingPage : PageLayout.PageLayout msg
viewLoadingPage =
    let
        content =
            PageContent.oneColumn
                [ div [ class "profile-page_page-content" ]
                    [ div [ class "profile_main-content" ] []
                    ]
                ]
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewErrorPage : UserHandle -> Http.Error -> AppDocument msg
viewErrorPage handle error =
    let
        page =
            case error of
                Http.BadStatus 404 ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ StatusMessage.bad "Error, page not found" [] |> StatusMessage.view ])
                        PageFooter.pageFooter

                _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn
                            [ StatusMessage.bad "Error, could not load page" [] |> StatusMessage.view
                            ]
                        )
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground
    in
    { pageId = "profile-page profile-page-error"
    , title = UserHandle.toString handle ++ " | Error"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        appDoc pageId pageHeader pageTitle page modal =
            { pageId = pageId
            , title = pageTitle
            , appHeader = AppHeader.appHeader AppHeader.None
            , pageHeader = pageHeader
            , page = PageLayout.view page
            , modal = modal
            }
    in
    case model of
        Loading handle ->
            appDoc
                "user-or-org-profile-page"
                Nothing
                (UserHandle.toString handle)
                viewLoadingPage
                Nothing

        UserProfile { user, page, mobileNavIsOpen } ->
            let
                ( page_, modal_ ) =
                    UserProfilePage.view appContext.session user page

                header =
                    Just
                        (UserPageHeader.view
                            appContext.session
                            ToggleMobileNav
                            mobileNavIsOpen
                            UserPageHeader.UserProfile
                            user.handle
                            user
                        )
            in
            appDoc
                "user-page user-profile-page"
                header
                (UserHandle.toString user.handle)
                (PageLayout.map UserProfilePageMsg page_)
                (Maybe.map (Html.map UserProfilePageMsg) modal_)

        OrgProfile { org, page, mobileNavIsOpen } ->
            let
                page_ =
                    OrgProfilePage.view org page

                header =
                    Just
                        (OrgPageHeader.view
                            ToggleMobileNav
                            mobileNavIsOpen
                            OrgPageHeader.OrgProfile
                            org.handle
                            org
                        )
            in
            appDoc
                "org-page org-profile-page"
                header
                (UserHandle.toString org.handle)
                (PageLayout.map OrgProfilePageMsg page_)
                Nothing

        Failure handle error ->
            viewErrorPage handle error
