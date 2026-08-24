module UnisonShare.Page.ProjectTicketsPage exposing (..)

import Html exposing (Html, div, h2, header, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
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
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Page.ErrorPage as ErrorPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated exposing (PageCursor(..), PageCursorParam, Paginated(..))
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectTicketFormModal as ProjectTicketFormModal
import UnisonShare.Route as Route exposing (..)
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.Ticket as Ticket exposing (Ticket)
import UnisonShare.Ticket.TicketRef as TicketRef
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)



-- MODEL


type ContribitionsModal
    = NoModal
    | SubmitTicketModal ProjectTicketFormModal.Model


type alias PaginatedTickets =
    Paginated Ticket


type Tab
    = Open (WebData PaginatedTickets)
    | Closed (WebData PaginatedTickets)


type alias Model =
    { modal : ContribitionsModal
    , tab : Tab
    }


init : AppContext -> ProjectRef -> ProjectTicketsRoute -> ( Model, Cmd Msg )
init appContext projectRef subRoute =
    let
        ( tab, status, cursor ) =
            case subRoute of
                Route.ProjectTicketsOpen c ->
                    ( Open Loading, TicketStatus.Open, c )

                Route.ProjectTicketsClosed c ->
                    ( Closed Loading, TicketStatus.Closed, c )
    in
    ( { modal = NoModal, tab = tab }
    , fetchProjectTickets appContext projectRef status cursor
    )



-- UPDATE


type Msg
    = FetchTicketsFinished TicketStatus (WebData PaginatedTickets)
    | ShowSubmitTicketModal
    | ProjectTicketFormModalMsg ProjectTicketFormModal.Msg
    | CloseModal


type OutMsg
    = NoOut
    | AddedTicket


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case msg of
        FetchTicketsFinished status res ->
            case ( model.tab, status ) of
                ( Open _, TicketStatus.Open ) ->
                    ( { model | tab = Open res }, Cmd.none, NoOut )

                ( Closed _, TicketStatus.Closed ) ->
                    ( { model | tab = Closed res }, Cmd.none, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

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
                    , NoOut
                    )

                Session.Anonymous ->
                    ( model, Cmd.none, NoOut )

        ProjectTicketFormModalMsg formMsg ->
            case ( appContext.session, model.modal ) of
                ( Session.SignedIn _, SubmitTicketModal formModel ) ->
                    let
                        ( projectTicketFormModal, cmd, out ) =
                            ProjectTicketFormModal.update appContext projectRef formMsg formModel

                        ( modal, tab, out_ ) =
                            case out of
                                ProjectTicketFormModal.None ->
                                    ( SubmitTicketModal projectTicketFormModal, model.tab, NoOut )

                                ProjectTicketFormModal.RequestToCloseModal ->
                                    ( NoModal, model.tab, NoOut )

                                ProjectTicketFormModal.Saved c ->
                                    let
                                        tab_ =
                                            case model.tab of
                                                Open tix ->
                                                    Open
                                                        (RemoteData.map (\(Paginated p) -> Paginated { p | items = c :: p.items }) tix)

                                                Closed tix ->
                                                    Closed
                                                        (RemoteData.map (\(Paginated p) -> Paginated { p | items = c :: p.items }) tix)
                                    in
                                    ( NoModal, tab_, AddedTicket )
                    in
                    ( { model | modal = modal, tab = tab }
                    , Cmd.map ProjectTicketFormModalMsg cmd
                    , out_
                    )

                _ ->
                    ( model, Cmd.none, NoOut )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )


updateSubPage : AppContext -> ProjectRef -> ProjectTicketsRoute -> Model -> ( Model, Cmd Msg )
updateSubPage appContext projectRef subRoute model =
    case subRoute of
        ProjectTicketsOpen cursor ->
            case model.tab of
                Open _ ->
                    ( model
                    , fetchProjectTickets appContext projectRef TicketStatus.Open cursor
                    )

                _ ->
                    init appContext projectRef (ProjectTicketsOpen Paginated.NoPageCursor)

        ProjectTicketsClosed cursor ->
            case model.tab of
                Closed _ ->
                    ( model
                    , fetchProjectTickets appContext projectRef TicketStatus.Closed cursor
                    )

                _ ->
                    init appContext projectRef (ProjectTicketsClosed Paginated.NoPageCursor)



-- EFFECTS


fetchProjectTickets : AppContext -> ProjectRef -> TicketStatus -> PageCursorParam -> Cmd Msg
fetchProjectTickets appContext projectRef status cursor =
    let
        mkPaginated prev next items =
            Paginated { prev = prev, next = next, items = items }

        decode =
            Decode.succeed mkPaginated
                |> optional "prevCursor" (Decode.map (PageCursor >> Just) string) Nothing
                |> optional "nextCursor" (Decode.map (PageCursor >> Just) string) Nothing
                |> required "items" (Decode.list Ticket.decode)
    in
    ShareApi.projectTickets projectRef status cursor
        |> HttpApi.toRequest decode
            (RemoteData.fromResult >> FetchTicketsFinished status)
        |> HttpApi.perform appContext.api



-- VIEW


viewPageTitle : Session -> ProjectDetails -> PageTitle.PageTitle Msg
viewPageTitle session project =
    let
        pt =
            PageTitle.title "Tickets"

        canSubmit =
            Session.isSignedIn session && (Project.canView project || Project.isPublic project)

        button =
            Button.iconThenLabel ShowSubmitTicketModal Icon.bug "New ticket"
                |> Button.emphasized
    in
    if canSubmit then
        PageTitle.withRightSide [ Button.view button ] pt

    else
        pt
            |> PageTitle.withRightSide
                [ div [ class "submit-ticket-disabled" ]
                    [ Tooltip.text "Sign in to submit a ticket"
                        |> Tooltip.tooltip
                        |> Tooltip.view
                            (button
                                |> Button.disabled
                                |> Button.view
                            )
                    ]
                ]


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
            case ticket.author of
                Just a ->
                    ByAt.byAt a ticket.createdAt

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
            [ byAt
                |> ByAt.withToClick Link.userProfile
                |> ByAt.view appContext.timeZone appContext.now
            ]
        ]


viewPageContent : AppContext -> ProjectDetails -> Tab -> PageContent Msg
viewPageContent appContext project tab =
    let
        viewEmptyState icon text_ =
            EmptyState.iconCloud
                (EmptyState.CircleCenterPiece
                    (div [ class "tickets-empty-state_icon" ] [ Icon.view icon ])
                )
                |> EmptyState.withContent [ h2 [] [ text text_ ] ]
                |> EmptyStateCard.view

        ( tabList, tickets, emptyState ) =
            case tab of
                Open tix ->
                    ( TabList.tabList []
                        (TabList.tab "Open" (Link.projectTicketsOpen project.ref Paginated.NoPageCursor))
                        [ TabList.tab "Closed" (Link.projectTicketsClosed project.ref Paginated.NoPageCursor) ]
                    , tix
                    , viewEmptyState Icon.conversation "There are currently no open tickets."
                    )

                Closed tix ->
                    ( TabList.tabList
                        [ TabList.tab "Open" (Link.projectTicketsOpen project.ref Paginated.NoPageCursor) ]
                        (TabList.tab "Closed" (Link.projectTicketsClosed project.ref Paginated.NoPageCursor))
                        []
                    , tix
                    , viewEmptyState Icon.merge "There are currently no merged tickets."
                    )

        divider =
            Divider.divider
                |> Divider.small
                |> Divider.withoutMargin

        content =
            case tickets of
                Success (Paginated p) ->
                    let
                        toLink =
                            case tab of
                                Open _ ->
                                    Link.projectTicketsOpen project.ref

                                Closed _ ->
                                    Link.projectTicketsClosed project.ref

                        items =
                            p.items
                                |> List.map (viewTicketRow appContext project.ref)
                                |> List.intersperse (Divider.view divider)
                    in
                    if List.isEmpty items then
                        emptyState

                    else
                        div []
                            [ Card.card items
                                |> Card.withClassName "project-tickets"
                                |> Card.asContained
                                |> Card.view
                            , Paginated.view toLink p
                            ]

                _ ->
                    -- The remaining RemoteData variants are handled by `view`
                    UI.nothing
    in
    PageContent.oneColumn [ TabList.view tabList, content ]
        |> PageContent.withPageTitle (viewPageTitle appContext.session project)


view : AppContext -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    let
        paginatedTickets =
            case model.tab of
                Open tix ->
                    tix

                Closed tix ->
                    tix
    in
    case paginatedTickets of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success _ ->
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
                (viewPageContent appContext project model.tab)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure e ->
            ( ErrorPage.view appContext.session e "tickets" "project-tickets"
            , Nothing
            )
