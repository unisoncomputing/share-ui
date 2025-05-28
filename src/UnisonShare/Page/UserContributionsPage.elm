module UnisonShare.Page.UserContributionsPage exposing (..)

import Code.Branch as Branch
import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Hash as Hash exposing (Hash)
import Dict
import Html exposing (Html, br, div, h2, p, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.SearchResults as SearchResults exposing (SearchResults)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Maybe.Extra as MaybeE
import RemoteData exposing (WebData)
import UI
import UI.Card as Card
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.KpiTag as KpiTag
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated
import UnisonShare.Project as Project exposing (Project)
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef as ProjectRef



-- MODEL


type alias ContributionProject =
    Project {}


type alias BranchSummary =
    Branch.BranchSummary ContributionProject


type alias ProjectContributions =
    ( ContributionProject, List Contribution )


type alias Contribution =
    { branchRef : BranchRef
    , project : ContributionProject
    , hash : Hash
    }


type ContributionsSearch
    = NotAsked String
    | Searching String (Maybe (SearchResults ProjectContributions))
    | Success String (SearchResults ProjectContributions)
    | Failure String Http.Error


type alias Model =
    { contributionsByProject : WebData (List ProjectContributions)
    , search : ContributionsSearch
    }


init : AppContext -> UserHandle -> ( Model, Cmd Msg )
init appContext handle =
    let
        params =
            { searchQuery = Nothing
            , projectRef = Nothing
            , limit = 100
            , cursor = Paginated.NoPageCursor
            }

        model =
            { contributionsByProject = RemoteData.Loading
            , search = NotAsked ""
            }
    in
    ( model, fetchContributions FetchContributionsFinished appContext handle params )



-- UPDATE


type Msg
    = FetchContributionsFinished (HttpResult (List Contribution))
    | UpdateSearchQuery String
    | FetchSearchResultsFinished String (HttpResult (List Contribution))
    | ClearSearch


update : AppContext -> UserHandle -> Msg -> Model -> ( Model, Cmd Msg )
update appContext handle msg model =
    case msg of
        FetchContributionsFinished contribs ->
            ( { model
                | contributionsByProject =
                    contribs
                        |> RemoteData.fromResult
                        |> RemoteData.map toContributionsByProject
              }
            , Cmd.none
            )

        UpdateSearchQuery query ->
            if String.length query <= 2 then
                ( { model | search = NotAsked query }, Cmd.none )

            else
                let
                    previousResults =
                        case model.search of
                            Searching _ results ->
                                results

                            Success _ results ->
                                Just results

                            _ ->
                                Nothing

                    cmd =
                        let
                            params =
                                { searchQuery = Just query
                                , projectRef = Nothing
                                , limit = 100
                                , cursor = Paginated.NoPageCursor
                                }
                        in
                        fetchContributions (FetchSearchResultsFinished query) appContext handle params
                in
                ( { model | search = Searching query previousResults }, cmd )

        ClearSearch ->
            ( { model | search = NotAsked "" }, Cmd.none )

        FetchSearchResultsFinished query result ->
            case model.search of
                Searching q _ ->
                    if q == query then
                        let
                            search =
                                case result of
                                    Ok contribs ->
                                        contribs
                                            |> toContributionsByProject
                                            |> SearchResults.fromList
                                            |> Success query

                                    Err err ->
                                        Failure query err
                        in
                        ( { model | search = search }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- HELPERS


toContributionsByProject : List Contribution -> List ProjectContributions
toContributionsByProject contribs =
    let
        group contrib acc =
            let
                key =
                    ProjectRef.toString contrib.project.ref

                update_ prev =
                    case prev of
                        Nothing ->
                            Just ( contrib.project, [ contrib ] )

                        Just ( pRef, cs ) ->
                            Just ( pRef, cs ++ [ contrib ] )
            in
            Dict.update key update_ acc
    in
    contribs
        |> List.foldl group Dict.empty
        |> Dict.values



-- EFFECTS


fetchContributions : (HttpResult (List Contribution) -> Msg) -> AppContext -> UserHandle -> ShareApi.UserBranchesParams -> Cmd Msg
fetchContributions toMsg appContext handle params =
    let
        branchSummariesToContributions : List BranchSummary -> List Contribution
        branchSummariesToContributions bs =
            List.map
                (\b ->
                    { branchRef = b.ref
                    , project = b.project
                    , hash = b.causalHash
                    }
                )
                bs
    in
    ShareApi.userBranches handle params
        |> HttpApi.toRequest (Decode.field "items" (Decode.list (Branch.decodeSummary Project.decode)))
            (Result.map branchSummariesToContributions >> toMsg)
        |> HttpApi.perform appContext.api


viewContribution : Contribution -> Html msg
viewContribution contribution =
    let
        branchTag =
            BranchRef.toTag contribution.branchRef
                |> Tag.withClick
                    (Link.projectBranchRoot
                        contribution.project.ref
                        contribution.branchRef
                    )
                |> Tag.large
    in
    div [ class "contribution" ]
        [ Tag.view branchTag
        , Hash.view contribution.hash
        ]


viewContributionsForProject : ( ContributionProject, List Contribution ) -> Html msg
viewContributionsForProject ( project, contributions ) =
    div [ class "contributions-for-project" ]
        [ ProjectListing.projectListing project
            |> ProjectListing.withClick
                Link.userProfile
                Link.projectOverview
            |> ProjectListing.view
        , div [ class "contribution-list" ] (List.map viewContribution contributions)
        ]


viewContentCard : List (Html msg) -> Html msg
viewContentCard content =
    Card.card content
        |> Card.asContained
        |> Card.withClassName "project-contributions"
        |> Card.view


viewSubHeaderLoading : Html msg
viewSubHeaderLoading =
    Placeholder.text
        |> Placeholder.subdued
        |> Placeholder.withLength Placeholder.Large
        |> Placeholder.view


viewLoading : List (Html msg)
viewLoading =
    let
        project =
            Placeholder.text
                |> Placeholder.tiny
                |> Placeholder.withLength Placeholder.Large
                |> Placeholder.view

        contrib len =
            Placeholder.text
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.withLength len
                |> Placeholder.view
    in
    [ viewSubHeader viewSubHeaderLoading viewSubHeaderLoading
    , viewContentCard
        [ div [ class "project-contributions_loading" ]
            [ div [ class "project-contributions_loading_project" ]
                [ project
                , div [ class "project-contributions_loading_contributions" ]
                    [ contrib Placeholder.Small
                    , contrib Placeholder.Tiny
                    , contrib Placeholder.Large
                    , contrib Placeholder.Medium
                    ]
                ]
            , div [ class "project-contributions_loading_project" ]
                [ project
                , div [ class "project-contributions_loading_contributions" ]
                    [ contrib Placeholder.Medium
                    , contrib Placeholder.Small
                    , contrib Placeholder.Huge
                    , contrib Placeholder.Large
                    ]
                ]
            ]
        ]
    ]


viewLoadingPage : PageLayout.PageLayout msg
viewLoadingPage =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn viewLoading)
        PageFooter.pageFooter


viewEmptyState : UserHandle -> List (Html Msg)
viewEmptyState handle =
    let
        emptyState =
            EmptyState.iconCloud (EmptyState.CircleCenterPiece (text "🐣"))
                |> EmptyState.withContent
                    [ p
                        [ class "no-contribs" ]
                        [ text (UserHandle.toString handle ++ " hasn't created any contributions yet.") ]
                    , p [] [ text "Check back later." ]
                    ]
    in
    [ viewSubHeader
        (div [ class "disabled-search-field" ] [ viewSearchField "" (TextField.TextFieldIcon Icon.search) ])
        UI.nothing
    , EmptyStateCard.view emptyState
    ]


viewEmptySearchResults : String -> List (Html Msg)
viewEmptySearchResults query =
    [ viewSubHeader (viewSearchField query (TextField.TextFieldIcon Icon.search)) UI.nothing
    , viewContentCard
        [ EmptyState.search
            |> EmptyState.withContent
                [ h2 [] [ text "No matches" ]
                , p []
                    [ text "We looked everywhere, but couldn't find any"
                    , br [] []
                    , text ("contributions matching \"" ++ query ++ "\".")
                    ]
                ]
            |> EmptyState.view
        ]
    ]


viewKpis : List ProjectContributions -> Html msg
viewKpis contributionsByProject =
    let
        numProjects =
            List.length contributionsByProject

        numContributions =
            contributionsByProject
                |> List.foldl (\( _, cs ) acc -> acc ++ cs) []
                |> List.length
    in
    div [ class "user-contributions-page_kpis" ]
        [ KpiTag.kpiTag "Contribution" numContributions
            |> KpiTag.withIcon Icon.branch
            |> KpiTag.view
        , KpiTag.kpiTag "Project" numProjects
            |> KpiTag.withIcon Icon.pencilRuler
            |> KpiTag.view
        ]


viewSearchField : String -> TextField.TextFieldIcon Msg -> Html Msg
viewSearchField value textFieldIcon =
    TextField.fieldWithoutLabel UpdateSearchQuery "Search branches" value
        |> TextField.withTextFieldIcon textFieldIcon
        |> TextField.withHelpText "E.g. \"main\", \"feature\"."
        |> TextField.withClear ClearSearch
        |> TextField.view


viewSubHeader : Html msg -> Html msg -> Html msg
viewSubHeader left right =
    div [ class "user-contributions-page_sub-header" ] [ left, right ]


view : UserHandle -> Model -> PageLayout.PageLayout Msg
view handle { contributionsByProject, search } =
    let
        content =
            case ( search, contributionsByProject ) of
                ( NotAsked q, RemoteData.Success contribsByProject ) ->
                    if List.isEmpty contribsByProject then
                        viewEmptyState handle

                    else
                        [ viewSubHeader
                            (viewSearchField q (TextField.TextFieldIcon Icon.search))
                            (viewKpis contribsByProject)
                        , viewContentCard (List.map viewContributionsForProject contribsByProject)
                        ]

                ( Searching q results, RemoteData.Success contribsByProject ) ->
                    let
                        contribsByProject_ =
                            MaybeE.unwrap contribsByProject SearchResults.toList results
                    in
                    [ viewSubHeader
                        (viewSearchField q (TextField.TextFieldStatusIndicator StatusIndicator.working))
                        viewSubHeaderLoading
                    , viewContentCard
                        (List.map viewContributionsForProject contribsByProject_
                            ++ [ div [ class "user-contributions-page_searching-overlay" ] [] ]
                        )
                    ]

                ( Success q results, _ ) ->
                    if SearchResults.isEmpty results then
                        viewEmptySearchResults q

                    else
                        [ viewSubHeader
                            (viewSearchField q (TextField.TextFieldIcon Icon.search))
                            (viewKpis (SearchResults.toList results))
                        , viewContentCard
                            (results
                                |> SearchResults.toList
                                |> List.map viewContributionsForProject
                            )
                        ]

                ( Failure q _, _ ) ->
                    [ viewSubHeader (viewSearchField q (TextField.TextFieldStatusIndicator StatusIndicator.bad)) UI.nothing
                    , viewContentCard [ StatusBanner.bad "Something broke on our end and we couldn't perform the search. Please try again." ]
                    ]

                ( NotAsked q, RemoteData.Failure _ ) ->
                    [ viewSubHeader (viewSearchField q (TextField.TextFieldStatusIndicator StatusIndicator.bad)) UI.nothing
                    , viewContentCard [ StatusBanner.bad "Something broke on our end and we couldn't perform the search. Please try again." ]
                    ]

                _ ->
                    viewLoading
    in
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn content
            |> PageContent.withPageTitle (PageTitle.title "Contributions")
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground
