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
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route exposing (OrgRoute)



-- MODEL


type SubPage
    = People
    | Settings


type alias Model =
    { org : WebData OrgSummary
    , subPage : SubPage
    , mobileNavIsOpen : Bool
    }


init : AppContext -> UserHandle -> OrgRoute -> ( Model, Cmd Msg )
init appContext handle _ =
    ( { org = Loading
      , subPage = People
      , mobileNavIsOpen = False
      }
    , fetchOrg appContext handle
    )



-- UPDATE


type Msg
    = FetchOrgFinished (WebData OrgSummary)
    | ToggleMobileNav


update : AppContext -> UserHandle -> OrgRoute -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ msg model =
    case ( model.subPage, msg ) of
        ( _, FetchOrgFinished o ) ->
            ( { model | org = o }, Cmd.none )

        ( _, ToggleMobileNav ) ->
            ( { model | mobileNavIsOpen = not model.mobileNavIsOpen }, Cmd.none )


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
        PageLayout.centeredNarrowLayout (PageContent.oneColumn [ text "loading..." ])
            PageFooter.pageFooter
            |> PageLayout.view
    , modal = Nothing
    }


view : AppContext -> UserHandle -> Model -> AppDocument Msg
view appContext handle model =
    {-
       let
           handle_ =
               UserHandle.toString handle

           orgPageHeader activeNavItem user =
               OrgPageHeader.view
                   ToggleMobileNav
                   model.mobileNavIsOpen
                   activeNavItem
                   handle
                   user
       in
    -}
    case model.org of
        NotAsked ->
            viewLoadingPage appContext model.subPage handle

        Loading ->
            viewLoadingPage appContext model.subPage handle

        Failure e ->
            viewErrorPage appContext model.subPage handle e

        Success _ ->
            viewLoadingPage appContext model.subPage handle
