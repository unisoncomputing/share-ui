module UnisonShare.Page.NotificationsPage exposing (..)

import Code.ProjectNameListing as ProjectNameListing
import Html exposing (Html, div, h1, h4, span, strong, text)
import Html.Attributes exposing (class, classList)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI
import UI.Avatar as Avatar
import UI.AvatarStack as AvatarStack
import UI.Card as Card
import UI.DateTime as DateTime
import UI.Divider as Divider
import UI.Form.Checkbox as Checkbox
import UI.Nudge as Nudge
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.TabList as TabList
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Contribution.ContributionRef as ContributionRef
import UnisonShare.Link as Link
import UnisonShare.Notification exposing (Notification, NotificationSubject(..))
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route exposing (NotificationsRoute(..))
import UnisonShare.Ticket.TicketRef as TicketRef



-- MODEL


type Paginated a
    = Paginated
        { cursor : String
        , perPage : Int
        , total : Int
        , items : List a
        }


type alias PaginatedNotifications =
    Paginated Notification


type NotificationSelection
    = NoSelection
    | AllNotifications
    | SubsetSelected (Set String)


type alias SubPageState =
    { notifications : WebData PaginatedNotifications
    , selection : NotificationSelection
    }


type SubPage
    = All SubPageState
    | Unread SubPageState
    | Archive SubPageState


type alias Model =
    SubPage


init : AppContext -> NotificationsRoute -> ( Model, Cmd Msg )
init appContext route =
    let
        notifications =
            Paginated
                { cursor = "asdf"
                , perPage = 12
                , total = 512
                , items =
                    [ { id = "asdf"
                      , projectRef = ProjectRef.unsafeFromString "unison" "base"
                      , title = "some notification title"
                      , subject = TicketSubject (TicketRef.unsafeFromString "1234")
                      , occurredAt = DateTime.unsafeFromISO8601 "2025-05-05T19:22:03.038Z"
                      , participants = []
                      , isUnread = True
                      }
                    , { id = "ghjhj"
                      , projectRef = ProjectRef.unsafeFromString "hojberg" "html"
                      , title = "something else"
                      , subject = ContributionSubject (ContributionRef.unsafeFromString "1234")
                      , occurredAt = DateTime.unsafeFromISO8601 "2025-05-03T19:22:03.038Z"
                      , participants = []
                      , isUnread = False
                      }
                    , { id = "ghjhj"
                      , projectRef = ProjectRef.unsafeFromString "hojberg" "html"
                      , title = "a third thing"
                      , subject = ContributionSubject (ContributionRef.unsafeFromString "1234")
                      , occurredAt = DateTime.unsafeFromISO8601 "2025-05-02T19:22:03.038Z"
                      , participants = []
                      , isUnread = False
                      }
                    ]
                }

        subPageState =
            { notifications = Success notifications
            , selection = NoSelection
            }
    in
    case route of
        NotificationsAll ->
            ( All subPageState, fetchNotifications appContext )

        NotificationsUnread ->
            ( Unread subPageState, fetchUnreadNotifications appContext )

        NotificationsArchive ->
            ( Archive subPageState, fetchArchivedNotifications appContext )



-- UPDATE


type Msg
    = SelectAll
    | FetchAllNotificationsFinished (WebData PaginatedNotifications)
    | ToggleSelection Notification
    | MarkSelectionAsUnread
    | MarkSelectionAsRead
    | ArchiveSelection
    | UnarchiveSelection


update : AppContext -> NotificationsRoute -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ msg model =
    case msg of
        SelectAll ->
            let
                selectAll_ state =
                    { state | selection = AllNotifications }
            in
            ( updateSubPageState selectAll_ model, Cmd.none )

        _ ->
            ( model, Cmd.none )


updateSubPageState : (SubPageState -> SubPageState) -> Model -> Model
updateSubPageState f model =
    case model of
        All s ->
            All (f s)

        Unread s ->
            Unread (f s)

        Archive s ->
            Archive (f s)



-- EFFECTS


fetchNotifications : AppContext -> Cmd Msg
fetchNotifications appContext =
    fetchNotifications_ appContext


fetchUnreadNotifications : AppContext -> Cmd Msg
fetchUnreadNotifications appContext =
    fetchNotifications_ appContext


fetchArchivedNotifications : AppContext -> Cmd Msg
fetchArchivedNotifications appContext =
    fetchNotifications_ appContext


fetchNotifications_ : AppContext -> Cmd Msg
fetchNotifications_ _ =
    Cmd.none


markNotificationsAsRead : AppContext -> Cmd Msg
markNotificationsAsRead _ =
    Cmd.none


markNotificationsAsUnread : AppContext -> Cmd Msg
markNotificationsAsUnread _ =
    Cmd.none


archiveNotifications : AppContext -> Cmd Msg
archiveNotifications _ =
    Cmd.none


unarchiveNotifications : AppContext -> Cmd Msg
unarchiveNotifications _ =
    Cmd.none



-- VIEW


isSelected : NotificationSelection -> Notification -> Bool
isSelected selection notification =
    case selection of
        NoSelection ->
            False

        AllNotifications ->
            True

        SubsetSelected set ->
            Set.member notification.id set


viewNotification : AppContext -> NotificationSelection -> Notification -> Html Msg
viewNotification appContext selection notification =
    let
        subject =
            case notification.subject of
                TicketSubject ticketRef ->
                    span []
                        [ text "Ticket: "
                        , strong [ class "notification-row_subject-ref" ] [ text (TicketRef.toString ticketRef) ]
                        ]

                ContributionSubject contributionRef ->
                    span []
                        [ text "Contribution: "
                        , strong [ class "notification-row_subject-ref" ] [ text (ContributionRef.toString contributionRef) ]
                        ]

        unreadDot =
            if notification.isUnread then
                Nudge.nudge
                    |> Nudge.emphasized
                    |> Nudge.view

            else
                UI.nothing

        isSelected_ =
            isSelected selection notification

        avatars =
            notification.participants
                |> List.map (\u -> Avatar.avatar u.avatarUrl u.name)
                |> AvatarStack.view

        projectListing =
            notification.projectRef
                |> ProjectRef.toProjectName
                |> ProjectNameListing.projectNameListing
                |> (\pl ->
                        if notification.isUnread then
                            pl

                        else
                            ProjectNameListing.verySubdued pl
                   )
                |> ProjectNameListing.view
    in
    div
        [ class "notification-row"
        , classList
            [ ( "notification-row_unread", notification.isUnread )
            , ( "notification-row_selected", isSelected_ )
            ]
        ]
        [ div [ class "notification-row_selection-and-details" ]
            [ div [ class "notification-row_selection" ]
                [ Checkbox.checkbox_ (Just (ToggleSelection notification)) isSelected_
                    |> Checkbox.view
                , unreadDot
                ]
            , div [ class "notification-row_details" ]
                [ div [ class "notification-row_details_context-and-subject" ]
                    [ projectListing
                    , span [ class "notification-row_details_subject" ]
                        [ subject ]
                    ]
                , h4 [ class "notification-row_details_title" ]
                    [ text notification.title
                    ]
                ]
            ]
        , div [ class "notification-row_participants" ] [ avatars ]
        , div [ class "notification-row_date" ]
            [ DateTime.view
                (DateTime.DistanceFrom appContext.now)
                appContext.timeZone
                notification.occurredAt
            ]
        ]


viewNotifications : AppContext -> NotificationSelection -> List Notification -> Html Msg
viewNotifications appContext selection notifications =
    div [ class "notifications" ]
        (notifications
            |> List.map (viewNotification appContext selection)
            |> List.intersperse
                (Divider.divider
                    |> Divider.small
                    |> Divider.withoutMargin
                    |> Divider.view
                )
        )


view_ : AppContext -> NotificationSelection -> WebData PaginatedNotifications -> Html Msg
view_ appContext selection paginatedNotifications =
    case paginatedNotifications of
        Success (Paginated { items }) ->
            div []
                [ Card.card [ viewNotifications appContext selection items ]
                    |> Card.asContained
                    |> Card.view
                ]

        _ ->
            text "TODO"


viewSelectionControls : Html Msg
viewSelectionControls =
    div [ class "notification_selection-controls" ] []


tabs : { all : TabList.Tab Msg, unread : TabList.Tab Msg, archive : TabList.Tab Msg }
tabs =
    { all = TabList.tab "All" Link.notificationsAll
    , unread = TabList.tab "Unread" Link.notificationsUnread
    , archive = TabList.tab "Archive" Link.notificationsArchive
    }


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        ( tabList, content ) =
            case model of
                All state ->
                    ( TabList.tabList [] tabs.all [ tabs.unread, tabs.archive ]
                    , view_ appContext state.selection state.notifications
                    )

                Unread state ->
                    ( TabList.tabList [ tabs.all ] tabs.unread [ tabs.archive ]
                    , view_ appContext state.selection state.notifications
                    )

                Archive state ->
                    ( TabList.tabList [ tabs.all, tabs.unread ] tabs.archive []
                    , view_ appContext state.selection state.notifications
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
