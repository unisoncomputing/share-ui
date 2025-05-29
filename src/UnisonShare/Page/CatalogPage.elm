module UnisonShare.Page.CatalogPage exposing (..)

import Browser.Navigation as Nav
import Html exposing (Html, div, footer, h1, p, strong, text)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra as StringE
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.Placeholder as Placeholder
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Catalog as Catalog exposing (Catalog, CatalogWithFeatured)
import UnisonShare.Link as Link
import UnisonShare.OmniSearch as OmniSearch
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project exposing (ProjectSummary)
import UnisonShare.Project.ProjectListing as ProjectListing
import Url



-- MODEL


type PageModal
    = NoModal
    | GetOnTheCatalogModal


type alias Model =
    { search : OmniSearch.Model
    , catalog : WebData Catalog
    , modal : PageModal
    }


init : AppContext -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init appContext searchQuery searchFilter =
    let
        ( omni, omniCmd ) =
            OmniSearch.init appContext searchQuery searchFilter

        model =
            { search = omni
            , catalog = Loading
            , modal = NoModal
            }
    in
    ( model, Cmd.batch [ fetchCatalog appContext, Cmd.map OmniSearchMsg omniCmd ] )



-- UPDATE


type Msg
    = FetchCatalogFinished (WebData Catalog)
    | RetryFetchCatalog
    | ShowGetOnTheCatalogModal
    | CloseModal
    | OmniSearchMsg OmniSearch.Msg


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        FetchCatalogFinished c ->
            ( { model | catalog = c }, Cmd.none )

        RetryFetchCatalog ->
            ( { model | catalog = Loading }, fetchCatalog appContext )

        ShowGetOnTheCatalogModal ->
            ( { model | modal = GetOnTheCatalogModal }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        OmniSearchMsg omniSearchMsg ->
            let
                ( search, cmd, omniSearchOut ) =
                    OmniSearch.update appContext omniSearchMsg model.search

                navCmd =
                    case omniSearchOut of
                        OmniSearch.NoOut ->
                            Cmd.none

                        OmniSearch.UpdateParams { query, filter } ->
                            let
                                queryString =
                                    [ StringE.nonEmpty query
                                        |> Maybe.map (\q -> "search=" ++ Url.percentEncode q)
                                    , StringE.nonEmpty filter
                                        |> Maybe.map (\f -> "filter=" ++ Url.percentEncode f)
                                    ]
                                        |> MaybeE.values
                                        |> String.join "&"
                                        |> StringE.nonEmpty
                                        |> Maybe.map (\qs -> "?" ++ qs)
                                        |> Maybe.withDefault ""
                            in
                            Nav.replaceUrl appContext.navKey queryString
            in
            ( { model | search = search }
            , Cmd.batch
                [ Cmd.map OmniSearchMsg cmd
                , navCmd
                ]
            )



-- EFFECTS


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


view_ : AppContext -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view_ appContext model =
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

        ( omniSearch, omniSearchModal ) =
            OmniSearch.view appContext model.search
    in
    ( PageLayout.heroLayout
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
                , Html.map OmniSearchMsg omniSearch
                ]
            )
        )
        (PageContent.oneColumn [ catalog ])
        PageFooter.pageFooter
    , Maybe.map (Html.map OmniSearchMsg) omniSearchModal
    )


view : AppContext -> Model -> AppDocument Msg
view appContext model =
    let
        ( page, pageModal ) =
            view_ appContext model

        modal =
            case model.modal of
                NoModal ->
                    pageModal

                GetOnTheCatalogModal ->
                    Just viewGetOnTheCatalogModal
    in
    { pageId = "catalog-page"
    , title = "Catalog"
    , pageHeader = Nothing
    , page = PageLayout.view page
    , appHeader = AppHeader.appHeader
    , modal = modal
    }
