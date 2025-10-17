module UnisonShare.Page.ProjectBranchesPage exposing (..)

import Code.Branch as Branch
import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html, br, div, footer, h2, p, span, strong, text)
import Html.Attributes exposing (class)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra as StringE
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.DateTime as DateTime
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary exposing (BranchSummary)
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated exposing (Paginated(..))
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route exposing (ProjectBranchesRoute(..))
import UnisonShare.Session as Session exposing (Session)



-- MODEL
{- TODO: group (contributor and maintainer), search, and empty state (however you got to that) -}


type alias Branches =
    WebData PaginatedBranches


type BranchesTab
    = AllBranches Branches
    | YourBranches Branches
    | MaintainerBranches Branches
    | ContributorBranches Branches


type alias DeleteBranch =
    WebData ()


type Modal
    = NoModal
    | DeleteBranchModal BranchRef DeleteBranch


type alias PaginatedBranches =
    Paginated.Paginated BranchSummary


type alias Model =
    { tab : BranchesTab
    , query : String
    , modal : Modal
    }


init : AppContext -> ProjectRef -> ProjectBranchesRoute -> Paginated.PageCursorParam -> ( Model, Cmd Msg )
init appContext projectRef branchesRoute cursor =
    let
        ( tab, kindFilter_ ) =
            case branchesRoute of
                ProjectBranchesAll ->
                    ( AllBranches Loading
                    , ShareApi.AllBranches Nothing
                    )

                ProjectBranchesYours ->
                    ( YourBranches Loading
                    , ShareApi.ContributorBranches (Session.handle appContext.session)
                    )

                ProjectBranchesMaintainer ->
                    ( MaintainerBranches Loading
                    , ShareApi.ProjectBranches
                    )

                ProjectBranchesContributor ->
                    ( ContributorBranches Loading
                    , ShareApi.ContributorBranches Nothing
                    )
    in
    ( { tab = tab, query = "", modal = NoModal }
    , fetchBranches appContext projectRef kindFilter_ Nothing cursor
    )



-- UPDATE


type Msg
    = FetchBranchesFinished (Maybe String) (WebData PaginatedBranches)
    | UpdateSearchQuery String
    | PerformSearch String
    | ClearSearch
    | ShowDeleteBranchModal BranchRef
    | CloseModal
    | YesDeleteBranch
    | DeleteBranchFinished (HttpResult ())


update : AppContext -> ProjectRef -> ProjectBranchesRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef _ msg model =
    case msg of
        FetchBranchesFinished q branches ->
            if q == StringE.nonEmpty model.query then
                ( { model | tab = mapTab (always branches) model.tab }, Cmd.none )

            else
                ( model, Cmd.none )

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
                        remove branches_ =
                            branches_
                                |> RemoteData.map
                                    (\(Paginated.Paginated paginated) ->
                                        Paginated.Paginated
                                            { paginated
                                                | items =
                                                    List.filter
                                                        (.ref >> BranchRef.equals branchRef >> not)
                                                        paginated.items
                                            }
                                    )
                    in
                    ( { model
                        | tab = mapTab remove model.tab
                        , modal = DeleteBranchModal branchRef (RemoteData.fromResult r)
                      }
                    , Util.delayMsg 1500 CloseModal
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateSearchQuery q ->
            let
                cmd =
                    if String.length q > 2 then
                        Util.delayMsg 300 (PerformSearch q)

                    else if String.isEmpty q then
                        fetchBranches
                            appContext
                            projectRef
                            (kindFilter appContext model.tab)
                            Nothing
                            Paginated.NoPageCursor

                    else
                        Cmd.none
            in
            ( { model | query = q }, cmd )

        PerformSearch q ->
            if model.query == q then
                ( { model | tab = mapTab (always Loading) model.tab }
                , fetchBranches appContext
                    projectRef
                    (kindFilter appContext model.tab)
                    (Just q)
                    Paginated.NoPageCursor
                )

            else
                ( model, Cmd.none )

        ClearSearch ->
            ( { model | query = "" }
            , fetchBranches
                appContext
                projectRef
                (kindFilter appContext model.tab)
                Nothing
                Paginated.NoPageCursor
            )



-- UPDATE HELPERS


kindFilter : AppContext -> BranchesTab -> ShareApi.ProjectBranchesKindFilter
kindFilter appContext tab =
    case tab of
        AllBranches _ ->
            ShareApi.AllBranches Nothing

        YourBranches _ ->
            ShareApi.ContributorBranches (Session.handle appContext.session)

        MaintainerBranches _ ->
            ShareApi.ProjectBranches

        ContributorBranches _ ->
            ShareApi.ContributorBranches Nothing


mapTab : (Branches -> Branches) -> BranchesTab -> BranchesTab
mapTab f tab =
    case tab of
        AllBranches branches ->
            AllBranches (f branches)

        YourBranches branches ->
            YourBranches (f branches)

        MaintainerBranches branches ->
            MaintainerBranches (f branches)

        ContributorBranches branches ->
            ContributorBranches (f branches)


updateSubPage : AppContext -> ProjectRef -> ProjectBranchesRoute -> Paginated.PageCursorParam -> Model -> ( Model, Cmd Msg )
updateSubPage appContext projectRef subRoute cursor model =
    case Debug.log "howdy" subRoute of
        ProjectBranchesAll ->
            case model.tab of
                AllBranches _ ->
                    ( model
                    , fetchBranches appContext
                        projectRef
                        (ShareApi.AllBranches Nothing)
                        Nothing
                        cursor
                    )

                _ ->
                    init appContext projectRef subRoute Paginated.NoPageCursor

        ProjectBranchesYours ->
            case model.tab of
                YourBranches _ ->
                    ( model
                    , fetchBranches appContext
                        projectRef
                        (ShareApi.ContributorBranches (Session.handle appContext.session))
                        Nothing
                        cursor
                    )

                _ ->
                    init appContext projectRef subRoute Paginated.NoPageCursor

        ProjectBranchesMaintainer ->
            case model.tab of
                MaintainerBranches _ ->
                    ( model
                    , fetchBranches appContext
                        projectRef
                        ShareApi.ProjectBranches
                        Nothing
                        cursor
                    )

                _ ->
                    init appContext projectRef subRoute Paginated.NoPageCursor

        ProjectBranchesContributor ->
            case model.tab of
                ContributorBranches _ ->
                    ( model
                    , fetchBranches appContext
                        projectRef
                        (ShareApi.ContributorBranches Nothing)
                        Nothing
                        cursor
                    )

                _ ->
                    init appContext projectRef subRoute Paginated.NoPageCursor



-- EFFECTS


fetchBranches : AppContext -> ProjectRef -> ShareApi.ProjectBranchesKindFilter -> Maybe String -> Paginated.PageCursorParam -> Cmd Msg
fetchBranches appContext projectRef kind query cursor =
    let
        params =
            { kind = kind
            , searchQuery = query
            , limit = 24
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
        |> HttpApi.toRequest decode (RemoteData.fromResult >> FetchBranchesFinished query)
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


viewLoading : Html msg
viewLoading =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view
    in
    Card.card
        [ shape Placeholder.Large
        , shape Placeholder.Small
        , shape Placeholder.Medium
        ]
        |> Card.asContained
        |> Card.view


viewLoadingPage : ProjectRef -> PageLayout msg
viewLoadingPage projectRef =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn [ viewLoading ]
            |> PageContent.withPageTitle (pageTitle projectRef)
        )
        PageFooter.pageFooter
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


viewPaginationControls : ProjectRef -> { a | prev : Maybe Paginated.PageCursor, next : Maybe Paginated.PageCursor } -> Html msg
viewPaginationControls projectRef cursors =
    let
        toLink cursor =
            Link.projectBranches projectRef cursor
    in
    Paginated.view toLink cursors


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


tabList : AppContext -> ProjectRef -> BranchesTab -> TabList.TabList msg
tabList appContext projectRef tab =
    let
        tabs =
            { all = TabList.tab "All" (Link.projectBranches projectRef Paginated.NoPageCursor)
            , yours = TabList.tab "Your branches" (Link.projectBranchesYours projectRef Paginated.NoPageCursor)
            , maintainer = TabList.tab "Maintainer branches" (Link.projectBranchesMaintainer projectRef Paginated.NoPageCursor)
            , contributor = TabList.tab "Contributor branches" (Link.projectBranchesContributor projectRef Paginated.NoPageCursor)
            }
    in
    case ( tab, appContext.session ) of
        ( AllBranches _, Session.SignedIn _ ) ->
            TabList.tabList [] tabs.all [ tabs.yours, tabs.maintainer, tabs.contributor ]

        ( AllBranches _, Session.Anonymous ) ->
            TabList.tabList [] tabs.all [ tabs.maintainer, tabs.contributor ]

        ( YourBranches _, _ ) ->
            TabList.tabList [ tabs.all ] tabs.yours [ tabs.maintainer, tabs.contributor ]

        ( MaintainerBranches _, Session.SignedIn _ ) ->
            TabList.tabList [ tabs.all, tabs.yours ] tabs.maintainer [ tabs.contributor ]

        ( MaintainerBranches _, Session.Anonymous ) ->
            TabList.tabList [ tabs.all ] tabs.maintainer [ tabs.contributor ]

        ( ContributorBranches _, Session.SignedIn _ ) ->
            TabList.tabList [ tabs.all, tabs.yours, tabs.maintainer ] tabs.contributor []

        ( ContributorBranches _, Session.Anonymous ) ->
            TabList.tabList [ tabs.all, tabs.maintainer ] tabs.contributor []


{-| TODO: add an empty state
-}
viewBranches : AppContext -> ProjectDetails -> Branches -> String -> Html Msg
viewBranches appContext project branches emptyStateMessage =
    let
        viewCard (Paginated p) =
            div []
                [ p.items
                    |> List.map (viewBranchRow appContext project)
                    |> div [ class "project-branches_list" ]
                    |> (\branchList ->
                            [ div [ class "project-branches_paginated-list" ]
                                [ branchList
                                ]
                            ]
                       )
                    |> Card.card
                    |> Card.asContained
                    |> Card.view
                , viewPaginationControls project.ref p
                ]
    in
    case branches of
        NotAsked ->
            viewLoading

        Loading ->
            viewLoading

        Success ((Paginated { items }) as paginated) ->
            if List.isEmpty items then
                EmptyState.iconCloud (EmptyState.IconCenterPiece Icon.branch)
                    |> EmptyState.withContent [ h2 [] [ text emptyStateMessage ] ]
                    |> EmptyStateCard.view

            else
                viewCard paginated

        Failure _ ->
            text "Couldn't load branches..."


view : AppContext -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    let
        searchField =
            TextField.fieldWithoutLabel UpdateSearchQuery "Search Branches" model.query
                |> TextField.withClear ClearSearch
                |> TextField.view

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                DeleteBranchModal branchRef del ->
                    if canDelete appContext.session project branchRef then
                        Just (viewDeleteBranchModal project.ref branchRef del)

                    else
                        Nothing

        tabContent =
            case model.tab of
                AllBranches branches ->
                    viewBranches appContext project branches "There are no branches for this project"

                YourBranches branches ->
                    case appContext.session of
                        Session.SignedIn _ ->
                            viewBranches appContext project branches "Your have no branches for this project"

                        _ ->
                            StatusBanner.info "Please sign in to view your branches."

                MaintainerBranches branches ->
                    viewBranches appContext project branches "There are no Maintainer branches for this project"

                ContributorBranches branches ->
                    viewBranches appContext project branches "There are no Contributor branches for this project"
    in
    ( PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ tabList appContext project.ref model.tab |> TabList.view
            , searchField
            , tabContent
            ]
            |> PageContent.withPageTitle (pageTitle project.ref)
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground
    , modal
    )
