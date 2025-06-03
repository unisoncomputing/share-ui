module UnisonShare.Page.NotificationsPage exposing (..)

import Code.BranchRef as BranchRef
import Code.ProjectNameListing as ProjectNameListing
import Html exposing (Html, div, footer, h1, h2, h4, span, strong, text)
import Html.Attributes exposing (class, classList)
import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI
import UI.Avatar as Avatar
import UI.Button as Button exposing (Button)
import UI.Card as Card
import UI.Click as Click
import UI.DateTime as DateTime
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Form.Checkbox as Checkbox
import UI.Form.CheckboxField as CheckboxField
import UI.Icon as Icon
import UI.Nudge as Nudge
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UnisonShare.Account exposing (Account)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Contribution.ContributionRef as ContributionRef
import UnisonShare.ErrorCard as ErrorCard
import UnisonShare.Link as Link
import UnisonShare.Notification as Notification exposing (Notification)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated exposing (PageCursor(..), PageCursorParam, Paginated(..))
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route exposing (NotificationsRoute(..))



-- MODEL


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


init : AppContext -> NotificationsRoute -> Account a -> ( Model, Cmd Msg )
init appContext route account =
    let
        subPageState =
            { notifications = Loading
            , selection = NoSelection
            }
    in
    case route of
        NotificationsAll cursor ->
            ( All subPageState
            , fetchNotifications appContext account cursor
            )

        NotificationsUnread cursor ->
            ( Unread subPageState
            , fetchUnreadNotifications appContext account cursor
            )

        NotificationsArchive cursor ->
            ( Archive subPageState
            , fetchArchivedNotifications appContext account cursor
            )



-- UPDATE


type Msg
    = ToggleSelectAll
    | FetchAllNotificationsFinished (WebData PaginatedNotifications)
    | ToggleSelection Notification
    | MarkSelectionAsUnread
    | MarkNotificationsAsUnreadFinished (HttpResult ())
    | MarkSelectionAsRead
    | MarkNotificationsAsReadFinished (HttpResult ())
    | ArchiveSelection
    | MarkNotificationsAsArchivedFinished (HttpResult ())
    | UnarchiveSelection
    | MarkNotificationsAsUnarchivedFinished (HttpResult ())


update : AppContext -> NotificationsRoute -> Account a -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ msg model =
    case msg of
        ToggleSelectAll ->
            let
                selectAll_ state =
                    let
                        selection =
                            case state.selection of
                                AllNotifications ->
                                    NoSelection

                                _ ->
                                    AllNotifications
                    in
                    { state | selection = selection }
            in
            ( updateSubPageState selectAll_ model, Cmd.none )

        ToggleSelection notification ->
            let
                toggleSelection state =
                    case state.selection of
                        NoSelection ->
                            { state | selection = SubsetSelected (Set.singleton notification.id) }

                        AllNotifications ->
                            let
                                updateSelections notifications =
                                    notifications
                                        |> (\(Paginated { items }) -> items)
                                        |> List.map .id
                                        |> List.filter (\nid -> nid /= notification.id)
                                        |> Set.fromList

                                selections_ =
                                    state.notifications
                                        |> RemoteData.map updateSelections
                                        |> RemoteData.withDefault Set.empty
                            in
                            { state | selection = SubsetSelected selections_ }

                        SubsetSelected selections ->
                            if Set.member notification.id selections then
                                let
                                    selections_ =
                                        Set.remove notification.id selections
                                in
                                if Set.isEmpty selections_ then
                                    { state | selection = NoSelection }

                                else
                                    { state | selection = SubsetSelected selections_ }

                            else
                                { state | selection = SubsetSelected (Set.insert notification.id selections) }
            in
            ( updateSubPageState toggleSelection model, Cmd.none )

        FetchAllNotificationsFinished notifications ->
            let
                updateNotifications _ =
                    { notifications = notifications
                    , selection = NoSelection
                    }
            in
            ( updateSubPageState updateNotifications model, Cmd.none )

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


fetchNotifications : AppContext -> Account a -> PageCursorParam -> Cmd Msg
fetchNotifications appContext account paginationCursor =
    fetchNotifications_ appContext account Nothing paginationCursor


fetchUnreadNotifications : AppContext -> Account a -> PageCursorParam -> Cmd Msg
fetchUnreadNotifications appContext account paginationCursor =
    fetchNotifications_ appContext account (Just Notification.Unread) paginationCursor


fetchArchivedNotifications : AppContext -> Account a -> PageCursorParam -> Cmd Msg
fetchArchivedNotifications appContext account paginationCursor =
    fetchNotifications_ appContext account (Just Notification.Archived) paginationCursor


fetchNotifications_ : AppContext -> Account a -> Maybe Notification.NotificationStatus -> PageCursorParam -> Cmd Msg
fetchNotifications_ appContext account status paginationCursorParam =
    let
        mkPaginated prev next items =
            Paginated { prev = prev, next = next, items = items }

        decode =
            Decode.succeed mkPaginated
                |> optional "prevCursor" (Decode.map (PageCursor >> Just) string) Nothing
                |> optional "nextCursor" (Decode.map (PageCursor >> Just) string) Nothing
                |> required "items" (Decode.list Notification.decode)
    in
    ShareApi.notifications account status paginationCursorParam
        |> HttpApi.toRequest
            decode
            (RemoteData.fromResult >> FetchAllNotificationsFinished)
        |> HttpApi.perform appContext.api


markNotificationsAsRead : AppContext -> Account a -> List String -> Cmd Msg
markNotificationsAsRead appContext account notificationIds =
    updateNotificationStatuses
        MarkNotificationsAsReadFinished
        appContext
        account
        notificationIds
        Notification.Read


markNotificationsAsUnread : AppContext -> Account a -> List String -> Cmd Msg
markNotificationsAsUnread appContext account notificationIds =
    updateNotificationStatuses
        MarkNotificationsAsUnreadFinished
        appContext
        account
        notificationIds
        Notification.Unread


archiveNotifications : AppContext -> Account a -> List String -> Cmd Msg
archiveNotifications appContext account notificationIds =
    updateNotificationStatuses
        MarkNotificationsAsArchivedFinished
        appContext
        account
        notificationIds
        Notification.Archived


unarchiveNotifications : AppContext -> Account a -> List String -> Cmd Msg
unarchiveNotifications appContext account notificationIds =
    updateNotificationStatuses MarkNotificationsAsUnarchivedFinished
        appContext
        account
        notificationIds
        Notification.Unread


updateNotificationStatuses : (HttpResult () -> Msg) -> AppContext -> Account a -> List String -> Notification.NotificationStatus -> Cmd Msg
updateNotificationStatuses toMsg appContext account notificationIds status =
    ShareApi.updateNotificationStatuses account notificationIds status
        |> HttpApi.toRequestWithEmptyResponse toMsg
        |> HttpApi.perform appContext.api



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
        ( title, event, projectRef ) =
            case notification.event.data of
                Notification.ProjectBranchUpdated eventData ->
                    ( "Branch update"
                    , span []
                        [ text "Branch: "
                        , strong [ class "notification-row_event-ref" ] [ text (BranchRef.toString eventData.branchRef) ]
                        ]
                    , eventData.projectRef
                    )

                Notification.ProjectContributionCreated eventData ->
                    ( "New contribution"
                    , span []
                        [ text "Contribution: "
                        , strong [ class "notification-row_event-ref" ] [ text (ContributionRef.toString eventData.contributionRef) ]
                        ]
                    , eventData.projectRef
                    )

        isUnread =
            Notification.isUnread notification

        unreadDot =
            if isUnread then
                Nudge.nudge
                    |> Nudge.emphasized
                    |> Nudge.view

            else
                UI.nothing

        isSelected_ =
            isSelected selection notification

        actor =
            notification.event.actor
                |> Maybe.map (\u -> Avatar.avatar u.avatarUrl u.name)
                |> Maybe.map Avatar.view
                |> Maybe.withDefault UI.nothing

        projectListing =
            projectRef
                |> ProjectRef.toProjectName
                |> ProjectNameListing.projectNameListing
                |> (\pl ->
                        if isUnread then
                            pl

                        else
                            ProjectNameListing.verySubdued pl
                   )
                |> ProjectNameListing.view
    in
    div
        [ class "notification-row"
        , classList
            [ ( "notification-row_unread", isUnread )
            , ( "notification-row_selected", isSelected_ )
            ]
        ]
        [ div [ class "notification-row_selection-and-details" ]
            [ div [ class "notification-row_selection" ]
                [ Checkbox.checkbox_
                    (Just (ToggleSelection notification))
                    isSelected_
                    |> Checkbox.view
                , unreadDot
                ]
            , div [ class "notification-row_details" ]
                [ div [ class "notification-row_details_context-and-event" ]
                    [ projectListing
                    , Icon.dot
                        |> Icon.withClass "notification-row_details_sep"
                        |> Icon.view
                    , span [ class "notification-row_details_event" ]
                        [ event ]
                    ]
                , h4 [ class "notification-row_details_title" ]
                    [ text title
                    ]
                ]
            ]
        , div [ class "notification-row_participants" ] [ actor ]
        , div [ class "notification-row_date" ]
            [ DateTime.view
                (DateTime.DistanceFrom appContext.now)
                appContext.timeZone
                notification.event.occurredAt
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


viewLoading : Html msg
viewLoading =
    Card.card (Placeholder.texts5 |> List.map Placeholder.view)
        |> Card.asContained
        |> Card.view


view_ : AppContext -> Model -> NotificationSelection -> WebData PaginatedNotifications -> Html Msg
view_ appContext model selection paginatedNotifications =
    case paginatedNotifications of
        NotAsked ->
            viewLoading

        Loading ->
            viewLoading

        Success (Paginated { prev, next, items }) ->
            if List.isEmpty items then
                let
                    message =
                        case model of
                            All _ ->
                                "You have no notifications"

                            Unread _ ->
                                "You have no unread notifications"

                            Archive _ ->
                                "You have no archived notifications"
                in
                EmptyState.iconCloud (EmptyState.IconCenterPiece Icon.bell)
                    |> EmptyState.withContent [ h2 [] [ text message ] ]
                    |> EmptyStateCard.view

            else
                div []
                    [ Card.card [ viewNotifications appContext selection items ]
                        |> Card.asContained
                        |> Card.view
                    , viewPaginationControls model { prev = prev, next = next }
                    ]

        Failure e ->
            ErrorCard.view appContext.session
                e
                "Notifications"
                "notifications-page_error"


viewSelectionControls : NotificationSelection -> List (Button Msg) -> Html Msg
viewSelectionControls selection controls =
    let
        isChecked =
            selection == AllNotifications

        controls_ =
            case selection of
                NoSelection ->
                    UI.nothing

                _ ->
                    div [ class "notifications_selection-controls_controls" ]
                        (List.map
                            (Button.small >> Button.emphasized >> Button.view)
                            controls
                        )
    in
    div [ class "notifications_selection-controls" ]
        [ CheckboxField.field "Select All" ToggleSelectAll isChecked
            |> CheckboxField.view
        , controls_
        ]


tabs : { all : TabList.Tab Msg, unread : TabList.Tab Msg, archive : TabList.Tab Msg }
tabs =
    { all = TabList.tab "All" (Link.notificationsAll Paginated.NoPageCursor)
    , unread = TabList.tab "Unread" (Link.notificationsUnread Paginated.NoPageCursor)
    , archive = TabList.tab "Archive" (Link.notificationsArchive Paginated.NoPageCursor)
    }


viewPaginationControls : Model -> { prev : Maybe PageCursor, next : Maybe PageCursor } -> Html Msg
viewPaginationControls model cursors =
    let
        link =
            case model of
                All _ ->
                    Link.notificationsAll

                Unread _ ->
                    Link.notificationsUnread

                Archive _ ->
                    Link.notificationsArchive

        paginationButton icon click =
            Button.icon_ click icon

        buttons =
            case ( cursors.prev, cursors.next ) of
                ( Just prev, Just next ) ->
                    [ paginationButton Icon.arrowLeft (link (Paginated.PrevPage prev))
                    , paginationButton Icon.arrowRight (link (Paginated.NextPage next))
                    ]

                ( Just prev, Nothing ) ->
                    [ paginationButton Icon.arrowLeft (link (Paginated.PrevPage prev))
                    , paginationButton Icon.arrowRight Click.disabled
                    ]

                ( Nothing, Just next ) ->
                    [ paginationButton Icon.arrowLeft Click.disabled
                    , paginationButton Icon.arrowRight (link (Paginated.NextPage next))
                    ]

                ( Nothing, Nothing ) ->
                    [ paginationButton Icon.arrowLeft Click.disabled
                    , paginationButton Icon.arrowRight Click.disabled
                    ]
    in
    footer [ class "pagination-controls" ] (List.map (Button.small >> Button.view) buttons)


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        ( tabList, selectionControls, content ) =
            case model of
                All state ->
                    ( TabList.tabList [] tabs.all [ tabs.unread, tabs.archive ]
                    , viewSelectionControls state.selection
                        [ Button.button MarkSelectionAsUnread "Mark as unread"
                        , Button.button MarkSelectionAsRead "Mark as read"
                        , Button.button ArchiveSelection "Archive"
                        ]
                    , view_ appContext model state.selection state.notifications
                    )

                Unread state ->
                    ( TabList.tabList [ tabs.all ] tabs.unread [ tabs.archive ]
                    , viewSelectionControls state.selection
                        [ Button.button MarkSelectionAsRead "Mark as read"
                        , Button.button ArchiveSelection "Archive"
                        ]
                    , view_ appContext model state.selection state.notifications
                    )

                Archive state ->
                    ( TabList.tabList [ tabs.all, tabs.unread ] tabs.archive []
                    , viewSelectionControls state.selection
                        [ Button.button ArchiveSelection "Unarchive"
                        ]
                    , view_ appContext model state.selection state.notifications
                    )
    in
    { pageId = "notifications-page"
    , title = "Notifications"
    , appHeader = AppHeader.appHeader
    , pageHeader = Nothing
    , page =
        PageLayout.centeredNarrowLayout
            (PageContent.oneColumn
                [ h1 [] [ text "Notifications" ]
                , TabList.view tabList
                , div [ class "notifications-page_content" ]
                    [ selectionControls
                    , content
                    ]
                ]
            )
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
            |> PageLayout.view
    , modal = Nothing
    }
