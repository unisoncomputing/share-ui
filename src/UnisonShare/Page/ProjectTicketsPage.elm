module UnisonShare.Page.ProjectTicketsPage exposing (..)

import Html exposing (Html, div, h2, header, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Page.ErrorPage as ErrorPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (Project)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectTicketFormModal as ProjectTicketFormModal
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.Ticket as Ticket exposing (Ticket)
import UnisonShare.Ticket.TicketRef as TicketRef
import UnisonShare.Ticket.TicketStatus as TicketStatus



-- MODEL


type ContribitionsModal
    = NoModal
    | SubmitTicketModal ProjectTicketFormModal.Model


type Tab
    = Open
    | Closed


type alias Model =
    { tickets : WebData (List Ticket)
    , modal : ContribitionsModal
    , tab : Tab
    }


init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    ( { tickets = Loading
      , modal = NoModal
      , tab = Open
      }
    , fetchProjectTickets appContext projectRef
    )



-- UPDATE


type Msg
    = FetchTicketsFinished (WebData (List Ticket))
    | ShowSubmitTicketModal
    | ProjectTicketFormModalMsg ProjectTicketFormModal.Msg
    | CloseModal
    | ChangeTab Tab


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef msg model =
    case msg of
        FetchTicketsFinished tickets ->
            ( { model | tickets = tickets }, Cmd.none )

        ShowSubmitTicketModal ->
            case appContext.session of
                Session.SignedIn _ ->
                    let
                        projectTicketFormModal =
                            ProjectTicketFormModal.init
                                ProjectTicketFormModal.Create
                    in
                    ( { model | modal = SubmitTicketModal projectTicketFormModal }
                    , Cmd.none
                    )

                Session.Anonymous ->
                    ( model, Cmd.none )

        ProjectTicketFormModalMsg formMsg ->
            case ( appContext.session, model.modal ) of
                ( Session.SignedIn _, SubmitTicketModal formModel ) ->
                    let
                        ( projectTicketFormModal, cmd, out ) =
                            ProjectTicketFormModal.update appContext projectRef formMsg formModel

                        ( modal, tickets ) =
                            case out of
                                ProjectTicketFormModal.None ->
                                    ( SubmitTicketModal projectTicketFormModal, model.tickets )

                                ProjectTicketFormModal.RequestToCloseModal ->
                                    ( NoModal, model.tickets )

                                ProjectTicketFormModal.Saved c ->
                                    ( NoModal, RemoteData.map (\cs -> c :: cs) model.tickets )
                    in
                    ( { model | modal = modal, tickets = tickets }
                    , Cmd.map ProjectTicketFormModalMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ChangeTab t ->
            ( { model | tab = t }, Cmd.none )



-- EFFECTS


fetchProjectTickets : AppContext -> ProjectRef -> Cmd Msg
fetchProjectTickets appContext projectRef =
    ShareApi.projectTickets projectRef
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list Ticket.decode))
            (RemoteData.fromResult >> FetchTicketsFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewPageTitle : Session -> Project a -> PageTitle.PageTitle Msg
viewPageTitle session project =
    let
        pt =
            PageTitle.title "Tickets"

        canSubmit =
            Session.hasProjectAccess project.ref session || (Session.isSignedIn session && Project.isPublic project)
    in
    if canSubmit then
        pt
            |> PageTitle.withRightSide
                [ Button.iconThenLabel ShowSubmitTicketModal Icon.bug "New ticket"
                    |> Button.emphasized
                    |> Button.view
                ]

    else
        pt


viewLoadingPage : PageLayout msg
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
                [ Card.card
                    [ shape Placeholder.Large
                    , shape Placeholder.Small
                    , shape Placeholder.Medium
                    ]
                    |> Card.asContained
                    |> Card.view
                ]
                |> PageContent.withPageTitle (PageTitle.title "Tickets")
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewTicketRow : AppContext -> ProjectRef -> Ticket -> Html Msg
viewTicketRow appContext projectRef ticket =
    let
        byAt =
            case ticket.authorHandle of
                Just h ->
                    ByAt.handleOnly h ticket.createdAt

                Nothing ->
                    ByAt.byUnknown ticket.createdAt

        numComments =
            if ticket.numComments > 0 then
                div [ class "num-comments" ]
                    [ Icon.view Icon.conversation
                    , text (String.fromInt ticket.numComments)
                    ]

            else
                UI.nothing
    in
    div [ class "ticket-row" ]
        [ header [ class "ticket-row_header" ]
            [ Click.view []
                [ h2 []
                    [ span [ class "ticket-row_ref" ]
                        [ text (TicketRef.toString ticket.ref)
                        ]
                    , text ticket.title
                    ]
                ]
                (Link.projectTicket projectRef ticket.ref)
            , numComments
            ]
        , div [ class "ticket-row_info" ]
            [ ByAt.view appContext.timeZone appContext.now byAt
            ]
        ]


viewPageContent : AppContext -> Project a -> Tab -> List Ticket -> PageContent Msg
viewPageContent appContext project tab tickets =
    let
        viewEmptyState icon text_ =
            EmptyState.iconCloud
                (EmptyState.CircleCenterPiece
                    (div [ class "tickets-empty-state_icon" ] [ Icon.view icon ])
                )
                |> EmptyState.withContent [ h2 [] [ text text_ ] ]
                |> EmptyStateCard.view

        ( tabList, status, emptyState ) =
            case tab of
                Open ->
                    ( TabList.tabList []
                        (TabList.tab "Open" (Click.onClick (ChangeTab Open)))
                        [ TabList.tab "Closed" (Click.onClick (ChangeTab Closed)) ]
                    , TicketStatus.Open
                    , viewEmptyState Icon.conversation "There are currently no open tickets."
                    )

                Closed ->
                    ( TabList.tabList
                        [ TabList.tab "Open" (Click.onClick (ChangeTab Open)) ]
                        (TabList.tab "Closed" (Click.onClick (ChangeTab Closed)))
                        []
                    , TicketStatus.Closed
                    , viewEmptyState Icon.merge "There are currently no merged tickets."
                    )

        divider =
            Divider.divider
                |> Divider.small
                |> Divider.withoutMargin

        content =
            tickets
                |> List.filter (\c -> c.status == status)
                |> List.map (viewTicketRow appContext project.ref)
                |> List.intersperse (Divider.view divider)

        card =
            if List.isEmpty content then
                emptyState

            else
                Card.card content
                    |> Card.withClassName "project-tickets"
                    |> Card.asContained
                    |> Card.view
    in
    PageContent.oneColumn [ TabList.view tabList, card ]
        |> PageContent.withPageTitle (viewPageTitle appContext.session project)


view : AppContext -> Project a -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    case model.tickets of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success tickets ->
            let
                modal =
                    case model.modal of
                        SubmitTicketModal form ->
                            Just
                                (Html.map ProjectTicketFormModalMsg
                                    (ProjectTicketFormModal.view "New ticket" form)
                                )

                        _ ->
                            Nothing
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext project model.tab tickets)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure e ->
            ( ErrorPage.view appContext.session e "tickets" "project-tickets"
            , Nothing
            )
