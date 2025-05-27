module UnisonShare.SearchBranchSheet exposing (..)

import Code.BranchRef as BranchRef
import Html exposing (Html, article, div, footer, h2, h3, h4, p, section, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search)
import Lib.SearchResults as SearchResults
import RemoteData exposing (WebData)
import UI
import UI.Click as Click
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonShare.Api as ShareApi exposing (ProjectBranchesKindFilter)
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)



-- MODEL


type alias Model =
    { search : Search BranchSummary
    , branchKindFilter : ProjectBranchesKindFilter
    }


init : ProjectBranchesKindFilter -> Model
init kindFilter =
    { search = Search.empty
    , branchKindFilter = kindFilter
    }



-- UPDATE


type Msg
    = UpdateSearchQuery String
    | PerformSearch String
    | ClearSearch
    | FetchSearchResultsFinished String (HttpResult (List BranchSummary))
    | SelectBranch BranchSummary
    | NoOp


type OutMsg
    = NoOutMsg
    | SelectBranchRequest BranchSummary


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case msg of
        UpdateSearchQuery q ->
            let
                newModel =
                    { model | search = Search.withQuery q model.search }

                ( sheet, cmd ) =
                    if Search.hasSubstantialQuery model.search then
                        ( newModel, Search.debounce (PerformSearch q) )

                    else
                        ( newModel, Cmd.none )
            in
            ( sheet, cmd, NoOutMsg )

        PerformSearch query ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.Searching query Nothing }
                , fetchBranches (FetchSearchResultsFinished query)
                    appContext
                    projectRef
                    { kind = model.branchKindFilter
                    , searchQuery = Just query
                    , limit = 10
                    , nextCursor = Nothing
                    , prevCursor = Nothing
                    }
                , NoOutMsg
                )

            else
                ( model, Cmd.none, NoOutMsg )

        ClearSearch ->
            ( { model | search = Search.reset model.search }, Cmd.none, NoOutMsg )

        FetchSearchResultsFinished query branches ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.fromResult model.search branches }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        NoOp ->
            ( model, Cmd.none, NoOutMsg )

        SelectBranch branch ->
            ( model, Cmd.none, SelectBranchRequest branch )



-- EFFECTS


fetchBranches :
    (HttpResult (List BranchSummary) -> Msg)
    -> AppContext
    -> ProjectRef
    -> ShareApi.ProjectBranchesParams
    -> Cmd Msg
fetchBranches doneMsg appContext projectRef params =
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list BranchSummary.decode))
            doneMsg
        |> HttpApi.perform appContext.api



-- VIEW
-- really this should be a<BranchRef>, but alas no higher kinded types


type alias Suggestions a =
    { data : WebData a
    , view : a -> List (Html Msg)
    }


viewBranch : BranchSummary -> Html Msg
viewBranch branch =
    BranchRef.toTag branch.ref
        |> Tag.large
        |> Tag.withClick (Click.onClick (SelectBranch branch))
        |> Tag.view


viewBranchList : String -> List BranchSummary -> Html Msg
viewBranchList title branches =
    section [ class "search-branch switch-branch_branch-list" ]
        [ h3 [] [ text title ]
        , div [ class "search-branch-sheet_branch-list_items" ] (List.map viewBranch branches)
        ]


view : String -> Suggestions a -> Maybe (Html Msg) -> Model -> Html Msg
view title suggestions footer_ model =
    let
        shape_ length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.tiny

        shape length =
            shape_ length
                |> Placeholder.subdued
                |> Placeholder.view

        shapeBright length =
            shape_ length
                |> Placeholder.view

        loading =
            [ div [ class "search-branch-sheet_recent-branches_loading" ]
                [ div [ class "search-branch-sheet_branches_loading-section" ]
                    [ shape Placeholder.Small
                    , div [ class "search-branch-sheet_branches_loading-list" ]
                        [ shapeBright Placeholder.Tiny
                        , shapeBright Placeholder.Medium
                        , shapeBright Placeholder.Small
                        ]
                    ]
                ]
            ]

        viewSuggestions data =
            case suggestions.view data of
                [] ->
                    []

                xs ->
                    [ div [ class "search-branch-sheet_suggestions" ] xs ]

        ( content, isSearching, query ) =
            case ( model.search, suggestions.data ) of
                ( Search.NotAsked q, RemoteData.Success data ) ->
                    ( viewSuggestions data
                    , False
                    , q
                    )

                ( Search.NotAsked q, RemoteData.Failure _ ) ->
                    ( [ StatusBanner.bad "Something broke on our end and we couldn't load the recent branches.\nPlease try again." ], False, q )

                ( Search.NotAsked q, RemoteData.NotAsked ) ->
                    ( loading, False, q )

                ( Search.NotAsked q, RemoteData.Loading ) ->
                    ( loading, False, q )

                ( Search.Searching q _, RemoteData.Success data ) ->
                    ( viewSuggestions data
                        ++ [ div [ class "search-branch-sheet_searching" ] [] ]
                    , True
                    , q
                    )

                ( Search.Searching q _, _ ) ->
                    ( [ div [ class "search-branch-sheet_searching" ]
                            [ div [ class "search-branch-sheet_branches_loading-list" ]
                                [ shapeBright Placeholder.Medium
                                , shapeBright Placeholder.Tiny
                                , shapeBright Placeholder.Small
                                ]
                            ]
                      ]
                    , True
                    , q
                    )

                ( Search.Success q sr, suggestions_ ) ->
                    let
                        results =
                            if SearchResults.isEmpty sr then
                                if q == "" then
                                    case suggestions_ of
                                        RemoteData.Success data ->
                                            viewSuggestions data

                                        _ ->
                                            []

                                else
                                    [ EmptyState.search
                                        |> EmptyState.onDark
                                        |> EmptyState.withContent
                                            [ div [ class "search-branch-sheet_no-results_message" ]
                                                [ h4 [] [ text "No matches" ]
                                                , p [] [ text ("We looked everywhere, but couldn't find any branches matching \"" ++ q ++ "\".") ]
                                                ]
                                            ]
                                        |> EmptyState.view
                                    ]

                            else
                                [ viewBranchList "Search results" (SearchResults.toList sr) ]
                    in
                    ( results, False, q )

                ( Search.Failure q _, _ ) ->
                    ( [ StatusBanner.bad "Something broke on our end and we couldn't perform the search. Please try again."
                      ]
                    , False
                    , q
                    )

        content_ =
            if List.isEmpty content then
                []

            else
                [ Divider.divider
                    |> Divider.small
                    |> Divider.onDark
                    |> Divider.withoutMargin
                    |> Divider.view
                , div [ class "search-branch-sheet_branches" ] content
                ]

        -- Currently this exists to prevent other keyboard centric interactions
        -- to take over from writing in the input field.
        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown (always NoOp)
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.attach

        footer__ =
            footer_
                |> Maybe.map (\f -> footer [ class "search-branch-sheet_more-link" ] [ f ])
                |> Maybe.withDefault UI.nothing
    in
    article [ class "search-branch-sheet", keyboardEvent ]
        [ h2 [] [ text title ]
        , Html.node "search"
            []
            ((TextField.fieldWithoutLabel UpdateSearchQuery "Search Branches" query
                |> TextField.withHelpText "Find a contributor branch by prefixing their handle, ex: \"@unison\"."
                |> TextField.withIconOrWorking Icon.search isSearching
                |> TextField.withClear ClearSearch
                |> TextField.view
             )
                :: content_
            )
        , footer__
        ]
