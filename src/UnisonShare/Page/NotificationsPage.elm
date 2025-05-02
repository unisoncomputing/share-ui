module UnisonShare.Page.NotificationsPage exposing (..)

import Html exposing (Html, div, h1, text)
import RemoteData exposing (RemoteData(..), WebData)
import UI.Card as Card
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.TabList as TabList
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route exposing (NotificationsRoute(..))



-- MODEL


type Paginated a
    = Paginated
        { cursor : String
        , perPage : Int
        , total : Int
        , items : List a
        }


type alias Notification =
    ()


type SubPage
    = All (WebData (Paginated Notification))
    | Unread (WebData (Paginated Notification))
    | Archive (WebData (Paginated Notification))


type alias Model =
    { subPage : SubPage
    }


init : AppContext -> NotificationsRoute -> ( Model, Cmd Msg )
init _ route =
    let
        ( subPage, cmd ) =
            case route of
                NotificationsAll ->
                    ( All Loading, Cmd.none )

                NotificationsUnread ->
                    ( Unread Loading, Cmd.none )

                NotificationsArchive ->
                    ( Archive Loading, Cmd.none )
    in
    ( { subPage = subPage }
    , cmd
    )



-- UPDATE


type Msg
    = NoOp


update : AppContext -> NotificationsRoute -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ model =
    ( model, Cmd.none )


updateSubPage : AppContext -> Model -> NotificationsRoute -> ( Model, Cmd Msg )
updateSubPage _ model _ =
    ( model, Cmd.none )



-- VIEW


tabs : { all : TabList.Tab Msg, unread : TabList.Tab Msg, archive : TabList.Tab Msg }
tabs =
    { all = TabList.tab "All" Link.notificationsAll
    , unread = TabList.tab "Unread" Link.notificationsUnread
    , archive = TabList.tab "Archive" Link.notificationsArchive
    }


view_ : String -> Html Msg
view_ s =
    div []
        [ Card.card [ text (s ++ " notifications") ]
            |> Card.asContained
            |> Card.view
        ]


view : AppContext -> Model -> AppDocument Msg
view _ model =
    let
        ( tabList, content ) =
            case model.subPage of
                All _ ->
                    ( TabList.tabList [] tabs.all [ tabs.unread, tabs.archive ]
                    , view_ "all"
                    )

                Unread _ ->
                    ( TabList.tabList [ tabs.all ] tabs.unread [ tabs.archive ]
                    , view_ "unread"
                    )

                Archive _ ->
                    ( TabList.tabList [ tabs.all, tabs.unread ] tabs.archive []
                    , view_ "archive"
                    )
    in
    { pageId = "notifications-page"
    , title = "Notifications"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page =
        PageLayout.centeredNarrowLayout
            (PageContent.oneColumn
                [ h1 [] [ text "Notifications" ]
                , TabList.view tabList
                , content
                ]
            )
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
            |> PageLayout.view
    , modal = Nothing
    }
