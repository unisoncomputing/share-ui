module UnisonShare.Page.ProjectBranchesPage exposing (..)

import Code.Branch as Branch
import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html, br, div, footer, p, span, strong, text)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary exposing (BranchSummary)
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Session as Session exposing (Session)



-- MODEL
{- TODO: Pagination, group (contributor and maintainer), search, and empty state (however you got to that) -}


type alias DeleteBranch =
    WebData ()


type Modal
    = NoModal
    | DeleteBranchModal BranchRef DeleteBranch


type alias PaginatedBranches =
    Paginated.Paginated BranchSummary


type alias Model =
    { branches : WebData PaginatedBranches
    , modal : Modal
    }


init : AppContext -> ProjectRef -> Paginated.PageCursorParam -> ( Model, Cmd Msg )
init appContext projectRef cursor =
    ( { branches = Loading, modal = NoModal }
    , fetchBranches appContext projectRef cursor
    )



-- UPDATE


type Msg
    = FetchBranchesFinished (WebData PaginatedBranches)
    | ShowDeleteBranchModal BranchRef
    | CloseModal
    | YesDeleteBranch
    | DeleteBranchFinished (HttpResult ())


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef msg model =
    case msg of
        FetchBranchesFinished branches ->
            ( { model | branches = branches }, Cmd.none )

        ShowDeleteBranchModal branchRef ->
            ( { model | modal = DeleteBranchModal branchRef NotAsked }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        YesDeleteBranch ->
            case model.modal of
                DeleteBranchModal branchRef _ ->
                    ( { model | modal = DeleteBranchModal branchRef Loading }
                    , deleteBranch appContext projectRef branchRef
                    )

                _ ->
                    ( model, Cmd.none )

        DeleteBranchFinished r ->
            case model.modal of
                DeleteBranchModal branchRef _ ->
                    let
                        branches =
                            model.branches
                                |> RemoteData.map
                                    (\(Paginated.Paginated branches_) ->
                                        Paginated.Paginated
                                            { branches_
                                                | items =
                                                    List.filter
                                                        (.ref >> BranchRef.equals branchRef >> not)
                                                        branches_.items
                                            }
                                    )
                    in
                    ( { model
                        | branches = branches
                        , modal = DeleteBranchModal branchRef (RemoteData.fromResult r)
                      }
                    , Util.delayMsg 1500 CloseModal
                    )

                _ ->
                    ( model, Cmd.none )



-- EFFECTS


fetchBranches : AppContext -> ProjectRef -> Paginated.PageCursorParam -> Cmd Msg
fetchBranches appContext projectRef cursor =
    let
        params =
            { kind = ShareApi.AllBranches Nothing
            , searchQuery = Nothing
            , limit = 12
            , cursor = cursor
            }

        mkPaginated prev next items =
            Paginated.Paginated { prev = prev, next = next, items = items }

        decode =
            Decode.succeed mkPaginated
                |> optional "prevCursor" (Decode.map (Paginated.PageCursor >> Just) Decode.string) Nothing
                |> optional "nextCursor" (Decode.map (Paginated.PageCursor >> Just) Decode.string) Nothing
                |> required "items" (Decode.list (Branch.decodeSummary Project.decode))
    in
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest decode (RemoteData.fromResult >> FetchBranchesFinished)
        |> HttpApi.perform appContext.api


deleteBranch : AppContext -> ProjectRef -> BranchRef -> Cmd Msg
deleteBranch appContext projectRef branchRef =
    ShareApi.deleteProjectBranch projectRef branchRef
        |> HttpApi.toRequestWithEmptyResponse DeleteBranchFinished
        |> HttpApi.perform appContext.api



-- VIEW


pageTitle : ProjectRef -> PageTitle.PageTitle msg
pageTitle projectRef =
    PageTitle.title "Project Branches"
        |> PageTitle.withDescription ("All branches for " ++ ProjectRef.toString projectRef)


viewLoadingPage : ProjectRef -> PageLayout msg
viewLoadingPage projectRef =
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
                |> PageContent.withPageTitle (pageTitle projectRef)
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewDeleteBranchModal : ProjectRef -> BranchRef -> DeleteBranch -> Html Msg
viewDeleteBranchModal projectRef branchRef deleting =
    let
        projectRef_ =
            ProjectRef.toString projectRef

        branchRef_ =
            BranchRef.toString branchRef

        ( statusBanner, overlay ) =
            case deleting of
                NotAsked ->
                    ( UI.nothing, UI.nothing )

                Loading ->
                    ( StatusBanner.working "Deleting..", div [ class "delete-branch-modal_overlay-deleting" ] [] )

                Success _ ->
                    ( UI.nothing
                    , div
                        [ class "delete-branch-modal_overlay-success"
                        ]
                        [ StatusIndicator.good |> StatusIndicator.large |> StatusIndicator.view
                        , div []
                            [ strong [] [ text branchRef_ ]
                            , br [] []
                            , text " successfully deleted"
                            ]
                        ]
                    )

                Failure _ ->
                    ( StatusBanner.bad "Delete branch failed", UI.nothing )

        content =
            div [ class "delete-branch-modal_content" ]
                [ p []
                    [ text "You're about to permanently delete the branch "
                    , strong [] [ text branchRef_ ]
                    , text " from "
                    , strong [] [ text projectRef_ ]
                    , text ", is that ok?"
                    ]
                , StatusBanner.info "Note that this will only delete the branch on Unison Share"
                , footer
                    [ class "delete-branch-modal_actions" ]
                    [ statusBanner
                    , Button.button CloseModal "Cancel"
                        |> Button.subdued
                        |> Button.medium
                        |> Button.view
                    , Button.button YesDeleteBranch "Yes, delete branch"
                        |> Button.critical
                        |> Button.medium
                        |> Button.view
                    ]
                , overlay
                ]
    in
    Modal.modal "delete-branch-modal" CloseModal (Modal.Content content)
        |> Modal.withHeader "Permanently Delete Branch?"
        |> Modal.view


viewAt : AppContext -> BranchSummary -> Html msg
viewAt appContext branch =
    let
        at_ location =
            let
                d =
                    if location == "tooltip" then
                        DateTime.FullDateTime

                    else
                        DateTime.DistanceFrom appContext.now
            in
            Keyed.node "div"
                []
                [ ( DateTime.toISO8601 branch.updatedAt ++ location
                  , span []
                        [ text
                            (DateTime.toString
                                d
                                appContext.timeZone
                                branch.updatedAt
                            )
                        ]
                  )
                ]

        tooltip =
            Tooltip.rich
                (div [ class "branch-updated-at_tooltip" ]
                    [ strong [] [ text (BranchRef.toString branch.ref) ]
                    , text "was last updated"
                    , at_ "tooltip"
                    ]
                )
                |> Tooltip.tooltip
    in
    Tooltip.view
        (div [ class "branch-updated-at" ]
            [ Icon.view Icon.clock
            , at_ "row"
            ]
        )
        tooltip


viewPaginationControls : ProjectRef -> { prev : Maybe Paginated.PageCursor, next : Maybe Paginated.PageCursor } -> Html msg
viewPaginationControls projectRef cursors =
    let
        link cursor =
            Link.projectBranches projectRef cursor

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


canDelete : Session -> ProjectDetails -> BranchRef -> Bool
canDelete session project branchRef =
    case branchRef of
        BranchRef.ContributorBranchRef h _ ->
            Session.isHandle h session

        _ ->
            Project.canMaintain project && project.defaultBranch /= Just branchRef


viewBranchRow : AppContext -> ProjectDetails -> BranchSummary -> Html Msg
viewBranchRow appContext project branch =
    let
        del =
            if canDelete appContext.session project branch.ref then
                Button.icon (ShowDeleteBranchModal branch.ref) Icon.trash
                    |> Button.small
                    |> Button.subdued
                    |> Button.view

            else
                UI.nothing
    in
    div [ class "project-branches_branch-row" ]
        [ div [ class "project-branches_branch-info" ]
            [ Click.view [] [ strong [] [ text (BranchRef.toString branch.ref) ] ] (Link.projectBranchRoot project.ref branch.ref)
            , viewAt appContext branch
            ]
        , del
        ]


{-| TODO: add an empty state
-}
viewPageContent : AppContext -> ProjectDetails -> PaginatedBranches -> PageContent Msg
viewPageContent appContext project (Paginated.Paginated { prev, next, items }) =
    let
        card =
            items
                |> List.map (viewBranchRow appContext project)
                |> div [ class "project-branches_list" ]
                |> List.singleton
                |> Card.card
                |> Card.asContained

        paginationControls =
            if List.isEmpty items then
                UI.nothing

            else
                viewPaginationControls project.ref { prev = prev, next = next }
    in
    PageContent.oneColumn
        [ Card.view card
        , paginationControls
        ]
        |> PageContent.withPageTitle (pageTitle project.ref)


view : AppContext -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    case model.branches of
        NotAsked ->
            ( viewLoadingPage project.ref, Nothing )

        Loading ->
            ( viewLoadingPage project.ref, Nothing )

        Success branches ->
            let
                modal =
                    case model.modal of
                        NoModal ->
                            Nothing

                        DeleteBranchModal branchRef del ->
                            if canDelete appContext.session project branchRef then
                                Just (viewDeleteBranchModal project.ref branchRef del)

                            else
                                Nothing
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext project branches)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure _ ->
            -- TODO
            ( PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ text "Couldn't load branches..." ])
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Nothing
            )
