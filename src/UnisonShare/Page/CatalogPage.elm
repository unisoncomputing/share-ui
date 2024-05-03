module UnisonShare.Page.CatalogPage exposing (..)

import Html exposing (Html, div, footer, h1, input, p, strong, table, tbody, td, text, tr)
import Html.Attributes exposing (autofocus, class, classList, placeholder)
import Html.Events exposing (onBlur, onFocus, onInput, onMouseDown)
import Json.Decode as Decode exposing (nullable, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search)
import Lib.SearchResults as SearchResults exposing (SearchResults(..))
import Lib.Util exposing (decodeTag)
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.Placeholder as Placeholder
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Catalog as Catalog exposing (Catalog, CatalogWithFeatured)
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (Project, ProjectSummary)
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route as Route
import UnisonShare.User as User exposing (UserSummary)



-- MODEL


{-| TODO: This should maybe include more fields and be more like ProjectSummary
-}
type alias ProjectSearchMatch =
    Project
        { summary : Maybe String
        }


type Match
    = UserMatch UserSummary
    | ProjectMatch ProjectSearchMatch


type alias CatalogSearch =
    Search Match


type PageModal
    = NoModal
    | GetOnTheCatalogModal


type alias Model =
    { search : CatalogSearch
    , hasFocus : Bool
    , catalog : WebData Catalog
    , keyboardShortcut : KeyboardShortcut.Model
    , modal : PageModal
    }


init : AppContext -> ( Model, Cmd Msg )
init appContext =
    let
        model =
            { search = Search.empty
            , hasFocus = True
            , catalog = Loading
            , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
            , modal = NoModal
            }
    in
    ( model, fetchCatalog appContext )



-- UPDATE


type Msg
    = UpdateQuery String
    | PerformSearch String
    | FetchCatalogFinished (WebData Catalog)
    | RetryFetchCatalog
    | UpdateFocus Bool
    | ClearQuery
    | SearchFinished String (HttpResult (List Match))
    | SelectMatch Match
    | ShowGetOnTheCatalogModal
    | CloseModal
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        FetchCatalogFinished c ->
            ( { model | catalog = c }, Cmd.none )

        RetryFetchCatalog ->
            ( { model | catalog = Loading }, fetchCatalog appContext )

        UpdateFocus hasFocus ->
            ( { model | hasFocus = hasFocus }, Cmd.none )

        UpdateQuery query ->
            let
                search =
                    Search.withQuery query model.search
            in
            if Search.hasSubstantialQuery search then
                ( { model | search = search }, Search.debounce (PerformSearch query) )

            else
                ( { model | search = search }, Cmd.none )

        PerformSearch query ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.toSearching model.search }, searchUsers appContext query )

            else
                ( model, Cmd.none )

        ClearQuery ->
            ( { model | search = Search.reset model.search }, Cmd.none )

        SearchFinished query results ->
            if Search.queryEquals query model.search then
                ( { model | search = Search.fromResult model.search results }, Cmd.none )

            else
                ( model, Cmd.none )

        SelectMatch match ->
            let
                cmd =
                    matchToNavigate appContext match
            in
            ( model, cmd )

        ShowGetOnTheCatalogModal ->
            ( { model | modal = GetOnTheCatalogModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        Keydown event ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.collect model.keyboardShortcut event.key

                cmd =
                    Cmd.map KeyboardShortcutMsg kCmd

                newModel =
                    { model | keyboardShortcut = keyboardShortcut }

                shortcut =
                    KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut event
            in
            case shortcut of
                Sequence _ Escape ->
                    ( { newModel | search = Search.reset model.search }, cmd )

                Sequence _ ArrowUp ->
                    ( { newModel | search = Search.searchResultsPrev model.search }, cmd )

                Sequence _ ArrowDown ->
                    ( { newModel | search = Search.searchResultsNext model.search }, cmd )

                Sequence _ Enter ->
                    case model.search of
                        Search.Success _ results ->
                            let
                                navigate =
                                    results
                                        |> SearchResults.focus
                                        |> Maybe.map (matchToNavigate appContext)
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( newModel, Cmd.batch [ cmd, navigate ] )

                        _ ->
                            ( newModel, cmd )

                Sequence (Just Semicolon) k ->
                    case Key.toNumber k of
                        Just n ->
                            let
                                navigate =
                                    Search.searchResults model.search
                                        |> Maybe.andThen (SearchResults.getAt (n - 1))
                                        |> Maybe.map (matchToNavigate appContext)
                                        |> Maybe.withDefault Cmd.none
                            in
                            ( newModel, Cmd.batch [ cmd, navigate ] )

                        Nothing ->
                            ( newModel, cmd )

                _ ->
                    ( newModel, cmd )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- EFFECTS


searchUsers : AppContext -> String -> Cmd Msg
searchUsers appContext query =
    let
        makeProjectSearchMatch ref visibility summary =
            { ref = ref, visibility = visibility, summary = summary }

        decodeProjectSearchMatch =
            Decode.succeed makeProjectSearchMatch
                |> required "projectRef" ProjectRef.decode
                |> required "visibility" Project.decodeVisibility
                |> required "summary" (nullable string)

        decodeMatch =
            Decode.oneOf
                [ when decodeTag ((==) "User") (Decode.map UserMatch User.decodeSummary)
                , when decodeTag ((==) "Project") (Decode.map ProjectMatch decodeProjectSearchMatch)
                ]
    in
    ShareApi.search query
        |> HttpApi.toRequest (Decode.list decodeMatch) (SearchFinished query)
        |> HttpApi.perform appContext.api


matchToNavigate : AppContext -> Match -> Cmd Msg
matchToNavigate appContext match =
    case match of
        UserMatch u ->
            u.handle |> Route.userProfile |> Route.navigate appContext.navKey

        ProjectMatch p ->
            p.ref |> Route.projectOverview |> Route.navigate appContext.navKey


fetchCatalog : AppContext -> Cmd Msg
fetchCatalog appContext =
    ShareApi.catalog
        |> HttpApi.toRequest Catalog.decode
            (RemoteData.fromResult >> FetchCatalogFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewCatalogProject : ProjectSummary -> Html msg
viewCatalogProject project =
    let
        listing =
            project
                |> ProjectListing.projectListing
                |> ProjectListing.large
                |> ProjectListing.withClick Link.userProfile Link.projectOverview
                |> ProjectListing.view

        summary =
            case project.summary of
                Just s ->
                    div [ class "catalog-project_summary" ] [ text s ]

                Nothing ->
                    UI.nothing
    in
    div [ class "catalog_project" ]
        [ listing
        , summary
        ]


viewCategory : ( String, List ProjectSummary ) -> Html msg
viewCategory ( category, projects ) =
    let
        projectLinks =
            projects
                |> List.map viewCatalogProject
    in
    Card.titled category [ div [ class "catalog_projects" ] projectLinks ]
        |> Card.view


{-| View a match in the dropdown list. Use `onMouseDown` instead of `onClick`
to avoid competing with `onBlur` on the input
-}
viewMatch : KeyboardShortcut.Model -> Match -> Bool -> Maybe Key -> Html Msg
viewMatch keyboardShortcut match isFocused shortcut =
    let
        shortcutIndicator =
            if isFocused then
                KeyboardShortcut.view keyboardShortcut (Sequence Nothing Key.Enter)

            else
                case shortcut of
                    Nothing ->
                        UI.nothing

                    Just key ->
                        KeyboardShortcut.view keyboardShortcut (Sequence (Just Key.Semicolon) key)
    in
    case match of
        UserMatch user ->
            tr
                [ classList [ ( "search-result", True ), ( "focused", isFocused ) ]
                , onMouseDown (SelectMatch match)
                ]
                [ td [ class "match-name" ]
                    [ div [ class "user-match" ]
                        [ ProfileSnippet.profileSnippet user |> ProfileSnippet.view
                        ]
                    ]
                , td [ class "category" ] [ text "User" ]
                , td [] [ div [ class "shortcut" ] [ shortcutIndicator ] ]
                ]

        ProjectMatch project ->
            let
                summary =
                    UI.viewMaybe
                        (\s ->
                            div [ class "project-match_summary" ]
                                [ text s ]
                        )
                        project.summary
            in
            tr
                [ classList [ ( "search-result", True ), ( "focused", isFocused ) ]
                , onMouseDown (SelectMatch match)
                ]
                [ td [ class "match-name" ]
                    [ div [ class "project-match" ]
                        [ ProjectListing.projectListing project |> ProjectListing.view
                        , summary
                        ]
                    ]
                , td [ class "category" ] [ text "Project" ]
                , td [] [ div [ class "shortcut" ] [ shortcutIndicator ] ]
                ]


indexToShortcut : Int -> Maybe Key
indexToShortcut index =
    let
        n =
            index + 1
    in
    if n > 9 then
        Nothing

    else
        n |> String.fromInt |> Key.fromString |> Just


viewMatches : KeyboardShortcut.Model -> SearchResults.Matches Match -> Html Msg
viewMatches keyboardShortcut matches =
    let
        matchItems =
            matches
                |> SearchResults.mapMatchesToList (\d f -> ( d, f ))
                |> List.indexedMap (\i ( d, f ) -> ( d, f, indexToShortcut i ))
                |> List.map (\( d, f, s ) -> viewMatch keyboardShortcut d f s)
    in
    table [] [ tbody [] matchItems ]


viewSearchResults : KeyboardShortcut.Model -> CatalogSearch -> Html Msg
viewSearchResults keyboardShortcut search =
    case search of
        Search.Success query r ->
            let
                resultsPane =
                    case r of
                        SearchResults.Empty ->
                            div [ class "empty-state" ] [ text ("No matches found for \"" ++ query ++ "\"") ]

                        SearchResults.SearchResults matches ->
                            viewMatches keyboardShortcut matches
            in
            div [ class "search-results" ] [ resultsPane ]

        _ ->
            UI.nothing


viewGetOnTheCatalogModal : Html Msg
viewGetOnTheCatalogModal =
    let
        content =
            Modal.Content
                (div
                    []
                    [ p [] [ text "To get listed on the Catalog make sure your project is public and have a release." ]
                    , p [] [ text "Make sure dependencies of your project are within a namespace called 'lib'. For instance, the 'base' dependency would be in 'lib.base.'" ]
                    , p []
                        [ text "Then post a message in the"
                        , Link.view "#libraries channel on the Unison Discord" Link.discord
                        , text "tagging Simon ("
                        , strong [] [ text "@hojberg" ]
                        , text ") with a link to your project."
                        ]
                    , footer [ class "modal-actions" ]
                        [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" |> Button.emphasized |> Button.view
                        ]
                    ]
                )
    in
    Modal.modal "get-on-the-catalog-modal" CloseModal content
        |> Modal.withHeader "Get listed on the Catalog"
        |> Modal.view


viewLoadingCatalog : Html msg
viewLoadingCatalog =
    let
        placeholderShape length =
            Placeholder.text
                |> Placeholder.subdued
                |> Placeholder.withLength length
                |> Placeholder.view

        viewLoadingCard =
            Card.card
                [ placeholderShape Placeholder.Medium
                , placeholderShape Placeholder.Small
                , placeholderShape Placeholder.Large
                , placeholderShape Placeholder.Small
                , placeholderShape Placeholder.Medium
                ]
                |> Card.view
    in
    div [ class "catalog" ]
        [ div [ class "categories" ]
            [ viewLoadingCard
            , viewLoadingCard
            , viewLoadingCard
            , viewLoadingCard
            , viewLoadingCard
            , viewLoadingCard
            ]
        ]


viewCategories : CatalogWithFeatured -> List (Html msg)
viewCategories categories =
    (categories.featured
        |> Maybe.map (\ps -> viewCategory ( "Featured", ps ))
        |> Maybe.map List.singleton
        |> Maybe.withDefault []
    )
        ++ List.map viewCategory categories.rest


view_ : Model -> PageLayout Msg
view_ model =
    let
        catalog =
            case model.catalog of
                NotAsked ->
                    viewLoadingCatalog

                Loading ->
                    viewLoadingCatalog

                Success catalog_ ->
                    let
                        categories =
                            catalog_
                                |> Catalog.asFeatured
                                |> viewCategories
                    in
                    div [ class "catalog" ]
                        [ div [ class "categories" ] categories
                        , div [ class "get-listed-cta" ] [ Button.iconThenLabel ShowGetOnTheCatalogModal Icon.window "Get listed on the Catalog" |> Button.view ]
                        ]

                Failure _ ->
                    div [ class "catalog catalog_error" ]
                        [ StatusMessage.bad "Couldn't load Catalog"
                            [ p [] [ text "Something broke on our end and the Catalog could not be loaded" ]
                            ]
                            |> StatusMessage.withCta (Button.iconThenLabel RetryFetchCatalog Icon.refresh "Try again" |> Button.medium)
                            |> StatusMessage.view
                        ]

        searchResults =
            if model.hasFocus then
                viewSearchResults model.keyboardShortcut model.search

            else
                UI.nothing

        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown Keydown
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.preventDefaultWhen
                    (\evt -> List.member evt.key [ ArrowUp, ArrowDown, Semicolon ])
                |> KeyboardEvent.attach
    in
    PageLayout.heroLayout
        (PageLayout.PageHero
            (div [ class "catalog-hero" ]
                [ h1 []
                    [ div []
                        [ strong [ class "explore" ] [ text "Explore" ]
                        , text ", "
                        , strong [ class "discover" ] [ text "Discover" ]
                        , text ", and "
                        , strong [ class "share" ] [ text "Share" ]
                        , text " Unison Code"
                        ]
                    , div [] [ text "Projects, libraries, documention, terms, and types" ]
                    ]
                , Html.node "search"
                    [ class "catalog-search", keyboardEvent ]
                    [ div [ class "search-field" ]
                        [ Icon.view Icon.search
                        , input
                            [ placeholder "Search for projects and users"
                            , onInput UpdateQuery
                            , autofocus True
                            , onBlur (UpdateFocus False)
                            , onFocus (UpdateFocus True)
                            ]
                            []
                        ]
                    , searchResults
                    ]
                ]
            )
        )
        (PageContent.oneColumn
            [ catalog
            ]
        )
        PageFooter.pageFooter


view : Model -> AppDocument Msg
view model =
    let
        page =
            model |> view_ |> PageLayout.view

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                GetOnTheCatalogModal ->
                    Just viewGetOnTheCatalogModal
    in
    { pageId = "catalog-page"
    , title = "Catalog"
    , pageHeader = Nothing
    , page = page
    , appHeader = AppHeader.appHeader AppHeader.Catalog
    , modal = modal
    }
