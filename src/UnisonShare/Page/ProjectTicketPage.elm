module UnisonShare.Page.ProjectTicketPage exposing (..)

import Html exposing (Html, div, header)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.Placeholder as Placeholder
import UnisonShare.Account as Account
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Link as Link
import UnisonShare.Page.ErrorPage as ErrorPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectTicketFormModal as ProjectTicketFormModal
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.Ticket as Ticket exposing (Ticket)
import UnisonShare.Ticket.TicketEvent as TicketEvent
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus exposing (TicketStatus(..))
import UnisonShare.Ticket.TicketTimeline as TicketTimeline
import UnisonShare.Timeline.TimelineEvent as TimelineEvent



-- MODEL


type UpdateStatus
    = TimelineNotReady
    | Idle
    | UpdatingStatus
    | UpdateStatusFailed Http.Error


type TicketModal
    = NoModal
    | EditModal ProjectTicketFormModal.Model


type alias Model =
    { ticket : WebData Ticket
    , timeline : TicketTimeline.Model
    , updateStatus : UpdateStatus
    , modal : TicketModal
    }


init : AppContext -> ProjectRef -> TicketRef -> ( Model, Cmd Msg )
init appContext projectRef ticketRef =
    let
        ( timeline, timelineCmd ) =
            TicketTimeline.init appContext projectRef ticketRef
    in
    ( { ticket = Loading
      , timeline = timeline
      , updateStatus = Idle
      , modal = NoModal
      }
    , Cmd.batch
        [ fetchTicket appContext projectRef ticketRef
        , Cmd.map TicketTimelineMsg timelineCmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchTicketFinished (WebData Ticket)
    | UpdateStatus TicketStatus
    | UpdateStatusFinished TicketStatus (HttpResult ())
    | ShowEditModal
    | ProjectTicketFormModalMsg ProjectTicketFormModal.Msg
    | CloseModal
    | TicketTimelineMsg TicketTimeline.Msg


update : AppContext -> ProjectRef -> TicketRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef ticketRef msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchTicketFinished ticket ->
            ( { model | ticket = ticket }, Cmd.none )

        UpdateStatus newStatus ->
            case model.ticket of
                Success ticket ->
                    ( { model | updateStatus = UpdatingStatus }
                    , updateTicketStatus appContext projectRef ticket.ref newStatus
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateStatusFinished newStatus res ->
            case appContext.session of
                Session.SignedIn me ->
                    case ( res, model.ticket ) of
                        ( Ok _, Success ticket ) ->
                            let
                                ticket_ =
                                    Success { ticket | status = newStatus }

                                ticketEvent =
                                    TicketEvent.StatusChange
                                        { newStatus = newStatus
                                        , oldStatus = Just ticket.status
                                        , timestamp = appContext.now
                                        , actor = Account.toUserSummary me
                                        }
                            in
                            ( { model
                                | ticket = ticket_
                                , timeline = TicketTimeline.addEvent model.timeline ticketEvent
                                , updateStatus = Idle
                              }
                            , Cmd.none
                            )

                        ( Err e, Success _ ) ->
                            ( { model | updateStatus = UpdateStatusFailed e }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                Session.Anonymous ->
                    ( model, Cmd.none )

        ShowEditModal ->
            case ( appContext.session, model.ticket ) of
                ( Session.SignedIn _, Success ticket ) ->
                    let
                        formModel =
                            ProjectTicketFormModal.init (ProjectTicketFormModal.Edit ticket)
                    in
                    ( { model | modal = EditModal formModel }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ProjectTicketFormModalMsg formMsg ->
            case ( appContext.session, model.modal ) of
                ( Session.SignedIn _, EditModal formModel ) ->
                    let
                        ( projectTicketFormModal, cmd, out ) =
                            ProjectTicketFormModal.update appContext
                                projectRef
                                formMsg
                                formModel

                        ( modal, ticket ) =
                            case out of
                                ProjectTicketFormModal.None ->
                                    ( EditModal projectTicketFormModal, model.ticket )

                                ProjectTicketFormModal.RequestToCloseModal ->
                                    ( NoModal, model.ticket )

                                ProjectTicketFormModal.Saved c ->
                                    -- TODO: also add a TicketEvent
                                    ( NoModal, Success c )
                    in
                    ( { model | modal = modal, ticket = ticket }
                    , Cmd.map ProjectTicketFormModalMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        TicketTimelineMsg timelineMsg ->
            let
                ( timeline, timelineCmd ) =
                    TicketTimeline.update appContext projectRef ticketRef timelineMsg model.timeline
            in
            ( { model | timeline = timeline }, Cmd.map TicketTimelineMsg timelineCmd )



-- EFFECTS


fetchTicket : AppContext -> ProjectRef -> TicketRef -> Cmd Msg
fetchTicket appContext projectRef ticketRef =
    ShareApi.projectTicket projectRef ticketRef
        |> HttpApi.toRequest Ticket.decode (RemoteData.fromResult >> FetchTicketFinished)
        |> HttpApi.perform appContext.api


updateTicketStatus :
    AppContext
    -> ProjectRef
    -> TicketRef
    -> TicketStatus
    -> Cmd Msg
updateTicketStatus appContext projectRef ticketRef newStatus =
    let
        update_ =
            ShareApi.ProjectTicketStatusUpdate newStatus
    in
    ShareApi.updateProjectTicket projectRef ticketRef update_
        |> HttpApi.toRequestWithEmptyResponse (UpdateStatusFinished newStatus)
        |> HttpApi.perform appContext.api



-- VIEW


viewTicket : Session -> ProjectRef -> UpdateStatus -> Ticket -> Html Msg
viewTicket session projectRef updateStatus ticket =
    let
        isContributor =
            ticket.author
                |> Maybe.map .handle
                |> Maybe.map (\h -> Session.isHandle h session)
                |> Maybe.withDefault False

        hasProjectAccess =
            Session.hasProjectAccess projectRef session

        className =
            if updateStatus == UpdatingStatus then
                "ticket-description ticket-description_updating"

            else
                "ticket-description"

        description =
            Markdown.toHtml [ class "definition-doc" ]
                ticket.description

        closeButton =
            if (hasProjectAccess || isContributor) && updateStatus /= TimelineNotReady then
                Button.iconThenLabel (UpdateStatus Closed) Icon.archive "Close"
                    |> Button.view

            else
                UI.nothing

        reopenButton =
            if hasProjectAccess || isContributor then
                Button.iconThenLabel (UpdateStatus Open) Icon.conversation "Re-open"
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        actions =
            case ticket.status of
                Open ->
                    [ div [ class "right-actions" ] [ closeButton ] ]

                Closed ->
                    [ div [ class "right-actions" ] [ reopenButton ] ]

        actions_ =
            if List.isEmpty actions then
                UI.nothing

            else
                div [ class "actions" ] actions
    in
    Card.card
        [ description, actions_ ]
        |> Card.asContained
        |> Card.withClassName className
        |> Card.view


viewStatusChangeEvent : DateTimeContext a -> TicketEvent.StatusChangeDetails -> List (Html Msg)
viewStatusChangeEvent dtContext { newStatus, oldStatus, actor, timestamp } =
    let
        byAt =
            ByAt.byAt actor timestamp
                |> ByAt.withToClick Link.userProfile
                |> ByAt.view dtContext.timeZone dtContext.now
    in
    case newStatus of
        Open ->
            let
                title =
                    case oldStatus of
                        Just Closed ->
                            "Re-opened"

                        _ ->
                            "Opened"
            in
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.conversation
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle title ]
                    , byAt
                    ]
                ]
            ]

        Closed ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.archive
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Closed" ]
                    , byAt
                    ]
                ]
            ]


viewPageContent :
    AppContext
    -> ProjectRef
    -> UpdateStatus
    -> Ticket
    -> TicketTimeline.Model
    -> PageContent Msg
viewPageContent appContext projectRef updateStatus ticket timeline =
    let
        timeline_ =
            TicketTimeline.view appContext projectRef timeline
    in
    PageContent.oneColumn
        [ viewTicket appContext.session projectRef updateStatus ticket
        , Html.map TicketTimelineMsg timeline_
        ]
        |> PageContent.withPageTitle (detailedPageTitle appContext ticket)


timeAgo : DateTimeContext a -> DateTime -> Html msg
timeAgo dateTimeContext t =
    DateTime.view (DateTime.DistanceFrom dateTimeContext.now) dateTimeContext.timeZone t


detailedPageTitle : AppContext -> Ticket -> PageTitle Msg
detailedPageTitle appContext ticket =
    let
        isContributor =
            ticket.author
                |> Maybe.map .handle
                |> Maybe.map (\h -> Session.isHandle h appContext.session)
                |> Maybe.withDefault False

        byAt =
            case ticket.author of
                Just a ->
                    ByAt.byAt a ticket.createdAt

                Nothing ->
                    ByAt.byUnknown ticket.createdAt

        rightSide =
            if isContributor then
                [ Button.iconThenLabel ShowEditModal Icon.writingPad "Edit"
                    |> Button.small
                    |> Button.outlined
                    |> Button.view
                ]

            else
                []
    in
    PageTitle.title ticket.title
        |> PageTitle.withLeftTitleText (TicketRef.toString ticket.ref)
        |> PageTitle.withDescription_
            (ByAt.view appContext.timeZone appContext.now byAt)
        |> PageTitle.withRightSide rightSide


viewLoadingPage : PageLayout Msg
viewLoadingPage =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        content =
            PageContent.oneColumn
                [ div []
                    [ Card.card
                        [ shape Placeholder.Large
                        , shape Placeholder.Small
                        , shape Placeholder.Medium
                        ]
                        |> Card.asContained
                        |> Card.view
                    ]
                ]
                |> PageContent.withPageTitle (PageTitle.title "Loading")
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewErrorPage : Session -> TicketRef -> Http.Error -> PageLayout Msg
viewErrorPage session _ error =
    ErrorPage.view session error "ticket" "project-ticket"


view : AppContext -> ProjectRef -> TicketRef -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext projectRef ticketRef model =
    case model.ticket of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success ticket ->
            let
                modal =
                    case model.modal of
                        -- TODO
                        EditModal form ->
                            Just
                                (Html.map ProjectTicketFormModalMsg
                                    (ProjectTicketFormModal.view "Save ticket" form)
                                )

                        _ ->
                            Nothing

                pageContent =
                    viewPageContent
                        appContext
                        projectRef
                        model.updateStatus
                        ticket
                        model.timeline
            in
            ( PageLayout.centeredNarrowLayout
                pageContent
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure e ->
            ( viewErrorPage appContext.session ticketRef e, Nothing )
