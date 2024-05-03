module UnisonShare.Page.ProjectBranchesPage exposing (..)

import Code.Branch as Branch
import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html, br, div, footer, p, span, strong, text)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import Json.Decode as Decode
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


type alias Model =
    { branches : WebData (List BranchSummary)
    , modal : Modal
    }


init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    ( { branches = Loading, modal = NoModal }, fetchBranches appContext projectRef )



-- UPDATE


type Msg
    = FetchBranchesFinished (WebData (List BranchSummary))
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
                                    (List.filter (.ref >> BranchRef.equals branchRef >> not))
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


fetchBranches : AppContext -> ProjectRef -> Cmd Msg
fetchBranches appContext projectRef =
    let
        params =
            { kind = ShareApi.AllBranches Nothing
            , searchQuery = Nothing
            , limit = 100
            , cursor = Nothing
            }
    in
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list (Branch.decodeSummary Project.decode)))
            (RemoteData.fromResult >> FetchBranchesFinished)
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


{-| TODO: wording here should talk about it only deleting it on Share
-}
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
            Keyed.node "div"
                []
                [ ( DateTime.toISO8601 branch.updatedAt ++ location
                  , span []
                        [ text
                            (DateTime.toString
                                (DateTime.DistanceFrom appContext.now)
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


canDelete : Session -> ProjectDetails -> BranchRef -> Bool
canDelete session project branchRef =
    case branchRef of
        BranchRef.ContributorBranchRef h _ ->
            Session.isHandle h session

        _ ->
            Session.hasProjectAccess project.ref session && project.defaultBranch /= Just branchRef


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


viewPageContent : AppContext -> ProjectDetails -> List BranchSummary -> PageContent Msg
viewPageContent appContext project branches =
    let
        card =
            branches
                |> List.map (viewBranchRow appContext project)
                |> div [ class "project-branches_list" ]
                |> List.singleton
                |> Card.card
                |> Card.asContained
    in
    PageContent.oneColumn
        [ Card.view card ]
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
