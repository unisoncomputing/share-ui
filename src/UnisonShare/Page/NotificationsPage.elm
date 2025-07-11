module UnisonShare.Page.NotificationsPage exposing (..)

import Code.BranchRef as BranchRef
import Code.ProjectNameListing as ProjectNameListing
import Code.Version as Version
import Html exposing (Html, div, h1, h2, h4, span, text)
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
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Contribution.ContributionRef as ContributionRef
import UnisonShare.Contribution.ContributionStatus as ContributionStatus
import UnisonShare.ErrorCard as ErrorCard
import UnisonShare.Link as Link
import UnisonShare.Notification as Notification exposing (Notification, NotificationStatus)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated exposing (PageCursor(..), PageCursorParam, Paginated(..))
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route as Route exposing (NotificationsRoute(..))
import UnisonShare.Ticket.TicketRef as TicketRef



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
    , updateSelection : WebData ()
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
        subPageState_ =
            { notifications = Loading
            , selection = NoSelection
            , updateSelection = NotAsked
            }
    in
    case route of
        NotificationsAll cursor ->
            ( All subPageState_
            , fetchNotifications appContext account cursor
            )

        NotificationsUnread cursor ->
            ( Unread subPageState_
            , fetchUnreadNotifications appContext account cursor
            )

        NotificationsArchive cursor ->
            ( Archive subPageState_
            , fetchArchivedNotifications appContext account cursor
            )



-- UPDATE


type NotificationsTab
    = AllTab
    | UnreadTab
    | ArchiveTab


type Msg
    = ChangeTab NotificationsTab
    | ToggleSelectAll
    | FetchAllNotificationsFinished (WebData PaginatedNotifications)
    | ToggleSelection Notification
    | UpdateSelection NotificationStatus
    | UpdateSelectionFinished NotificationStatus (HttpResult ())


type OutMsg
    = NoOutMsg
    | UpdatedNotificationStatuses
    | UpdateLastActiveNotificationsTab AppContext.LastActiveNotificationsTab


update : AppContext -> NotificationsRoute -> Account a -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext _ account msg model =
    case msg of
        ChangeTab tab ->
            let
                ( newRoute, out ) =
                    case tab of
                        AllTab ->
                            ( Route.notificationsAll Paginated.NoPageCursor
                            , UpdateLastActiveNotificationsTab AppContext.AllNotifications
                            )

                        UnreadTab ->
                            ( Route.notificationsUnread Paginated.NoPageCursor
                            , UpdateLastActiveNotificationsTab AppContext.UnreadNotifications
                            )

                        _ ->
                            ( Route.notificationsArchive Paginated.NoPageCursor
                            , NoOutMsg
                            )
            in
            ( model, Route.navigate appContext.navKey newRoute, out )

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
            ( updateSubPageState selectAll_ model, Cmd.none, NoOutMsg )

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
            ( updateSubPageState toggleSelection model, Cmd.none, NoOutMsg )

        FetchAllNotificationsFinished notifications ->
            let
                updateNotifications _ =
                    { notifications = notifications
                    , selection = NoSelection
                    , updateSelection = NotAsked
                    }
            in
            ( updateSubPageState updateNotifications model, Cmd.none, NoOutMsg )

        -- When marking is finished we should redirect to the current page without cursors and thus reload data
        -- should also refresh the account endpoint to make update the little dot on the notifications icon
        -- when its updating, the page should be blocked from interactivity.
        -- MarkNotificationsAsUnreadFinished (HttpResult ())
        UpdateSelection status ->
            let
                subPageState_ =
                    subPageState model

                ids =
                    case ( subPageState_.notifications, subPageState_.selection ) of
                        ( Success (Paginated { items }), AllNotifications ) ->
                            List.map .id items

                        ( Success (Paginated _), SubsetSelected ids_ ) ->
                            Set.toList ids_

                        _ ->
                            []

                updateNotificationStatus notification =
                    if List.member notification.id ids then
                        { notification | status = status }

                    else
                        notification

                notifications =
                    case subPageState_.notifications of
                        Success (Paginated paginated) ->
                            paginated.items
                                |> List.map updateNotificationStatus
                                |> (\items -> Paginated { paginated | items = items })
                                |> Success

                        _ ->
                            subPageState_.notifications
            in
            if List.isEmpty ids then
                ( model, Cmd.none, NoOutMsg )

            else
                let
                    update_ subState =
                        { subState
                            | notifications = notifications
                            , updateSelection = Loading
                            , selection = NoSelection
                        }
                in
                ( updateSubPageState update_ model
                , updateNotificationStatuses appContext account ids status
                , NoOutMsg
                )

        UpdateSelectionFinished status result ->
            case result of
                Ok _ ->
                    let
                        update_ notifications_ subState =
                            { subState
                                | notifications = Maybe.withDefault subState.notifications notifications_
                                , updateSelection = Success ()
                            }

                        -- We refresh and reset any pagination Except for when
                        -- we are on the "all" page and are just marking
                        -- read/unread (since this wont effect the number of
                        -- notifications show)
                        ( refresh, notifications ) =
                            case ( model, status ) of
                                ( All _, Notification.Read ) ->
                                    ( Nothing, Nothing )

                                ( All _, Notification.Unread ) ->
                                    ( Nothing, Nothing )

                                ( All _, _ ) ->
                                    ( Just (Route.NotificationsAll Paginated.NoPageCursor), Just Loading )

                                ( Unread _, _ ) ->
                                    ( Just (Route.NotificationsUnread Paginated.NoPageCursor), Just Loading )

                                ( Archive _, _ ) ->
                                    ( Just (Route.NotificationsArchive Paginated.NoPageCursor), Just Loading )
                    in
                    ( updateSubPageState (update_ notifications) model
                    , refresh
                        |> Maybe.map Route.Notifications
                        |> Maybe.map (Route.navigate appContext.navKey)
                        |> Maybe.withDefault Cmd.none
                    , UpdatedNotificationStatuses
                    )

                Err e ->
                    let
                        update_ subState =
                            { subState | updateSelection = Failure e }
                    in
                    ( updateSubPageState update_ model, Cmd.none, NoOutMsg )


subPageState : Model -> SubPageState
subPageState model =
    case model of
        All s ->
            s

        Unread s ->
            s

        Archive s ->
            s


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
                |> required "items" Notification.decodeList
    in
    ShareApi.notifications account status paginationCursorParam
        |> HttpApi.toRequest
            decode
            (RemoteData.fromResult >> FetchAllNotificationsFinished)
        |> HttpApi.perform appContext.api


updateNotificationStatuses : AppContext -> Account a -> List String -> Notification.NotificationStatus -> Cmd Msg
updateNotificationStatuses appContext account notificationIds status =
    ShareApi.updateNotificationStatuses account notificationIds status
        |> HttpApi.toRequestWithEmptyResponse (UpdateSelectionFinished status)
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
        { kind, title, link, event, projectRef } =
            case notification.event.data of
                Notification.ProjectBranchUpdated eventData ->
                    { kind = "Branch update"
                    , title = BranchRef.toString eventData.branchRef
                    , link = Link.projectBranchRoot eventData.projectRef eventData.branchRef
                    , event =
                        Link.projectBranchRoot eventData.projectRef eventData.branchRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Branch: " ++ BranchRef.toString eventData.branchRef) ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectContributionCreated eventData ->
                    { kind = "New contribution"
                    , title = eventData.title
                    , link = Link.projectContribution eventData.projectRef eventData.contributionRef
                    , event =
                        Link.projectContribution eventData.projectRef eventData.contributionRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Contribution: " ++ ContributionRef.toString eventData.contributionRef)
                                , ContributionStatus.view eventData.status
                                ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectContributionUpdated eventData ->
                    { kind = "Updated contribution"
                    , title = eventData.title
                    , link = Link.projectContribution eventData.projectRef eventData.contributionRef
                    , event =
                        Link.projectContribution eventData.projectRef eventData.contributionRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Contribution: " ++ ContributionRef.toString eventData.contributionRef)
                                , ContributionStatus.view eventData.status
                                ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectContributionComment eventData ->
                    { kind = "New comment"
                    , title = eventData.title
                    , link = Link.projectContribution eventData.projectRef eventData.contributionRef
                    , event =
                        Link.projectContribution eventData.projectRef eventData.contributionRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Contribution: " ++ ContributionRef.toString eventData.contributionRef)
                                , ContributionStatus.view eventData.status
                                ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectTicketCreated eventData ->
                    { kind = "New ticket"
                    , title = eventData.title
                    , link = Link.projectTicket eventData.projectRef eventData.ticketRef
                    , event =
                        Link.projectTicket eventData.projectRef eventData.ticketRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Ticket: " ++ TicketRef.toString eventData.ticketRef) ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectTicketUpdated eventData ->
                    { kind = "Updated ticket"
                    , title = eventData.title
                    , link = Link.projectTicket eventData.projectRef eventData.ticketRef
                    , event =
                        Link.projectTicket eventData.projectRef eventData.ticketRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Ticket: " ++ TicketRef.toString eventData.ticketRef) ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectTicketComment eventData ->
                    { kind = "New comment"
                    , title = eventData.title
                    , link = Link.projectTicket eventData.projectRef eventData.ticketRef
                    , event =
                        Link.projectTicket eventData.projectRef eventData.ticketRef
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Ticket: " ++ TicketRef.toString eventData.ticketRef) ]
                    , projectRef = eventData.projectRef
                    }

                Notification.ProjectReleaseCreated eventData ->
                    { kind = "New release"
                    , title = Version.toString eventData.version
                    , link = Link.projectRelease eventData.projectRef eventData.version
                    , event =
                        Link.projectRelease eventData.projectRef eventData.version
                            |> Click.view
                                [ class "notification-row_event-ref" ]
                                [ text ("Release: " ++ Version.toString eventData.version) ]
                    , projectRef = eventData.projectRef
                    }

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
                |> ProjectNameListing.withClick
                    Link.userProfile
                    (\_ -> Link.projectOverview projectRef)
                |> ProjectNameListing.small
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
                    , Icon.boldDot
                        |> Icon.withClass "notification-row_details_sep"
                        |> Icon.view
                    , span [ class "notification-row_details_event" ]
                        [ event ]
                    ]
                , h4 [ class "notification-row_details_title" ] [ Link.view title link ]
                ]
            ]
        , div [ class "notification-row_right" ]
            [ div [ class "notification-row_kind" ] [ text kind ]
            , div [ class "notification-row_participants" ] [ actor ]
            , div [ class "notification-row_date" ]
                [ DateTime.view
                    (DateTime.DistanceFrom appContext.now)
                    appContext.timeZone
                    notification.event.occurredAt
                ]
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
                            (Button.small >> Button.view)
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
    { all = TabList.tab "All" (Click.onClick (ChangeTab AllTab))
    , unread = TabList.tab "Unread" (Click.onClick (ChangeTab UnreadTab))
    , archive = TabList.tab "Archive" (Click.onClick (ChangeTab ArchiveTab))
    }


viewPaginationControls : Model -> { prev : Maybe PageCursor, next : Maybe PageCursor } -> Html Msg
viewPaginationControls model cursors =
    let
        toLink =
            case model of
                All _ ->
                    Link.notificationsAll

                Unread _ ->
                    Link.notificationsUnread

                Archive _ ->
                    Link.notificationsArchive
    in
    Paginated.view toLink cursors


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        ( tabList, selectionControls, content ) =
            case model of
                All state ->
                    ( TabList.tabList [] tabs.all [ tabs.unread, tabs.archive ]
                    , viewSelectionControls state.selection
                        [ Button.button (UpdateSelection Notification.Unread) "Mark as unread"
                        , Button.button (UpdateSelection Notification.Read) "Mark as read"
                        , Button.button (UpdateSelection Notification.Archived) "Archive"
                        ]
                    , view_ appContext model state.selection state.notifications
                    )

                Unread state ->
                    ( TabList.tabList [ tabs.all ] tabs.unread [ tabs.archive ]
                    , viewSelectionControls state.selection
                        [ Button.button (UpdateSelection Notification.Read) "Mark as read"
                        , Button.button (UpdateSelection Notification.Archived) "Archive"
                        ]
                    , view_ appContext model state.selection state.notifications
                    )

                Archive state ->
                    ( TabList.tabList [ tabs.all, tabs.unread ] tabs.archive []
                    , viewSelectionControls state.selection
                        [ Button.button (UpdateSelection Notification.Read) "Unarchive"
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
