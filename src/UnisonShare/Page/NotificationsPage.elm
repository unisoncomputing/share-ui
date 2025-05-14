module UnisonShare.Page.NotificationsPage exposing (..)

import Code.BranchRef as BranchRef
import Code.ProjectNameListing as ProjectNameListing
import Html exposing (Html, div, h1, h2, h4, span, strong, text)
import Html.Attributes exposing (class, classList)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI
import UI.Avatar as Avatar
import UI.Button as Button exposing (Button)
import UI.Card as Card
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
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route exposing (NotificationsRoute(..))



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


init : AppContext -> NotificationsRoute -> Account a -> ( Model, Cmd Msg )
init appContext route account =
    let
        subPageState =
            { notifications = Loading
            , selection = NoSelection
            }
    in
    case route of
        NotificationsAll ->
            ( All subPageState, fetchNotifications appContext account )

        NotificationsUnread ->
            ( Unread subPageState, fetchUnreadNotifications appContext account )

        NotificationsArchive ->
            ( Archive subPageState, fetchArchivedNotifications appContext account )



-- UPDATE


type Msg
    = ToggleSelectAll
    | FetchAllNotificationsFinished (WebData (List Notification))
    | ToggleSelection Notification
    | MarkSelectionAsUnread
    | MarkSelectionAsRead
    | ArchiveSelection
    | UnarchiveSelection


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
                toPaginatedNotifications notifications_ =
                    Paginated
                        { cursor = "TODO"
                        , perPage = 24
                        , total = 200
                        , items = notifications_
                        }

                updateNotifications _ =
                    { notifications = RemoteData.map toPaginatedNotifications notifications
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


fetchNotifications : AppContext -> Account a -> Cmd Msg
fetchNotifications appContext account =
    fetchNotifications_ appContext account


fetchUnreadNotifications : AppContext -> Account a -> Cmd Msg
fetchUnreadNotifications appContext account =
    fetchNotifications_ appContext account


fetchArchivedNotifications : AppContext -> Account a -> Cmd Msg
fetchArchivedNotifications appContext account =
    fetchNotifications_ appContext account


fetchNotifications_ : AppContext -> Account a -> Cmd Msg
fetchNotifications_ appContext account =
    ShareApi.notifications account
        |> HttpApi.toRequest
            (Decode.field "notifications" (Decode.list Notification.decode))
            (RemoteData.fromResult >> FetchAllNotificationsFinished)
        |> HttpApi.perform appContext.api


markNotificationsAsRead : AppContext -> Account a -> Cmd Msg
markNotificationsAsRead _ _ =
    Cmd.none


markNotificationsAsUnread : AppContext -> Account a -> Cmd Msg
markNotificationsAsUnread _ _ =
    Cmd.none


archiveNotifications : AppContext -> Account a -> Cmd Msg
archiveNotifications _ _ =
    Cmd.none


unarchiveNotifications : AppContext -> Account a -> Cmd Msg
unarchiveNotifications _ _ =
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


view_ : AppContext -> NotificationSelection -> WebData PaginatedNotifications -> Html Msg
view_ appContext selection paginatedNotifications =
    case paginatedNotifications of
        NotAsked ->
            viewLoading

        Loading ->
            viewLoading

        Success (Paginated { items }) ->
            if List.isEmpty items then
                EmptyState.iconCloud (EmptyState.IconCenterPiece Icon.bell)
                    |> EmptyState.withContent [ h2 [] [ text "You have no notifications" ] ]
                    |> EmptyStateCard.view

            else
                Card.card [ viewNotifications appContext selection items ]
                    |> Card.asContained
                    |> Card.view

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
    { all = TabList.tab "All" Link.notificationsAll
    , unread = TabList.tab "Unread" Link.notificationsUnread
    , archive = TabList.tab "Archive" Link.notificationsArchive
    }


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
                    , view_ appContext state.selection state.notifications
                    )

                Unread state ->
                    ( TabList.tabList [ tabs.all ] tabs.unread [ tabs.archive ]
                    , viewSelectionControls state.selection
                        [ Button.button MarkSelectionAsRead "Mark as read"
                        , Button.button ArchiveSelection "Archive"
                        ]
                    , view_ appContext state.selection state.notifications
                    )

                Archive state ->
                    ( TabList.tabList [ tabs.all, tabs.unread ] tabs.archive []
                    , viewSelectionControls state.selection
                        [ Button.button ArchiveSelection "Unarchive"
                        ]
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
                , selectionControls
                , content
                ]
            )
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
            |> PageLayout.view
    , modal = Nothing
    }
