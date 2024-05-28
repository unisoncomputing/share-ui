{-
   # OmniSearch

   There are 3 kinds of searches we perform (3 characters are required):

   ## Entity Search

   * Find Users or Projects.
   * Can only be triggered by prefixing the query with an "@"

   ## Definition Search

   * Search any definition either with or withour a filter (user or project).
   * Can only be triggered by a query that isn't prefixed with an "@".
   * Triggers when a "Name Search" returns (by searching the first result of
     the names), and when a name is commited to with Tab or Space.

   ## Name Search

   * Word based query (when the word isn't prefixed by a "@")
   * Searches types and term names (aka "tokens") that are mentioned by a definition
   * Definitions are searched automatically by the first name result even if it
     isn't commited to (allowing the user to see the definitions that match
     without having to do an extra step).
   * Commiting to a name (with Tab or Space) uses that name to search a definition

-}


module UnisonShare.OmniSearch exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Perspective as Perspective
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, classList, placeholder, spellcheck, type_, value)
import Html.Events exposing (onBlur, onFocus, onInput)
import Json.Decode as Decode exposing (nullable, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search(..))
import Lib.SearchResults as SearchResults
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util exposing (decodeTag)
import List.Extra as ListE
import Markdown
import Maybe.Extra as MaybeE
import UI
import UI.Click as Click
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.ProfileSnippet as ProfileSnippet
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Project as Project
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import UnisonShare.User as User



-- MODEL


type alias ProjectSearchMatch =
    Project.Project { summary : Maybe String }


type EntityMatch
    = ProjectMatch ProjectSearchMatch
    | UserMatch User.UserDetails


type DefinitionMatchType
    = TermMatch
    | TypeMatch
    | DataConstructorMatch
    | AbilityConstructorMatch


type alias DefinitionSearchMatch =
    { type_ : DefinitionMatchType
    , displayName : FQN
    , fqn : FQN
    , projectRef : ProjectRef
    , branchRef : BranchRef
    }


type MainSearch
    = NoSearch
    | EntitySearch (Search EntityMatch)
    | DefinitionSearch (Search DefinitionSearchMatch)


type Filter
    = NoFilter
    | ProjectFilter ProjectRef
    | UserFilter UserHandle


type alias Model =
    { fieldValue : String
    , filter : Filter
    , search : MainSearch
    , nameSearch : Search String
    , hasFocus : Bool
    , keyboardShortcut : KeyboardShortcut.Model
    }


init : AppContext -> Model
init appContext =
    { fieldValue = ""
    , filter = NoFilter
    , search = NoSearch
    , nameSearch = NotAsked ""
    , hasFocus = False
    , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
    }



-- UPDATE


type Msg
    = UpdateFieldValue String
    | EntitySearchFinished { query : String, results : HttpResult (List EntityMatch) }
    | DefinitionSearchFinished { query : String, results : HttpResult (List DefinitionSearchMatch) }
    | NameSearchFinished { query : String, results : HttpResult (List String) }
    | ClearFilter
    | UpdateFocus Bool
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg )
update appContext msg model =
    case msg of
        UpdateFieldValue value ->
            updateForValue appContext model value

        EntitySearchFinished res ->
            case ( model.search, res.results ) of
                ( EntitySearch (Searching q _), Ok matches ) ->
                    ( { model | search = EntitySearch (Success q (SearchResults.fromList matches)) }, Cmd.none )

                ( EntitySearch (Searching q _), Err e ) ->
                    ( { model | search = EntitySearch (Failure q e) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DefinitionSearchFinished res ->
            case ( model.search, res.results ) of
                ( DefinitionSearch (Searching q _), Ok matches ) ->
                    ( { model | search = DefinitionSearch (Success q (SearchResults.fromList matches)) }, Cmd.none )

                ( DefinitionSearch (Searching q _), Err e ) ->
                    ( { model | search = DefinitionSearch (Failure q e) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NameSearchFinished res ->
            if String.endsWith res.query model.fieldValue then
                case ( model.nameSearch, res.results ) of
                    ( Searching q _, Ok matches ) ->
                        let
                            results =
                                SearchResults.fromList matches

                            ( search, cmd ) =
                                SearchResults.focus results
                                    |> Maybe.map (\_ -> valueWithFocusedName model.fieldValue results)
                                    |> Maybe.map
                                        (\q_ ->
                                            ( DefinitionSearch (Searching q_ Nothing)
                                            , searchDefinitions appContext model.filter q_
                                            )
                                        )
                                    |> Maybe.withDefault ( model.search, Cmd.none )
                        in
                        ( { model | nameSearch = Success q results, search = search }, cmd )

                    ( Searching q _, Err e ) ->
                        ( { model | nameSearch = Failure q e }, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ClearFilter ->
            updateForValue appContext { model | filter = NoFilter } model.fieldValue

        UpdateFocus hasFocus ->
            ( { model | hasFocus = hasFocus }, Cmd.none )

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
                    -- Full reset
                    ( { newModel | search = NoSearch, fieldValue = "", nameSearch = NotAsked "" }, cmd )

                Sequence _ ArrowUp ->
                    case model.search of
                        EntitySearch s ->
                            ( { newModel | search = EntitySearch (Search.searchResultsPrev s) }, cmd )

                        DefinitionSearch s ->
                            ( { newModel | search = DefinitionSearch (Search.searchResultsPrev s) }, cmd )

                        _ ->
                            ( newModel, cmd )

                Sequence _ ArrowDown ->
                    case model.search of
                        EntitySearch s ->
                            ( { newModel | search = EntitySearch (Search.searchResultsNext s) }, cmd )

                        DefinitionSearch s ->
                            ( { newModel | search = DefinitionSearch (Search.searchResultsNext s) }, cmd )

                        _ ->
                            ( newModel, cmd )

                Sequence _ Enter ->
                    let
                        navCmd =
                            case model.search of
                                EntitySearch s ->
                                    case Maybe.andThen SearchResults.focus (Search.searchResults s) of
                                        Just (UserMatch u) ->
                                            Route.navigate appContext.navKey (Route.userProfile u.handle)

                                        Just (ProjectMatch p) ->
                                            Route.navigate appContext.navKey (Route.projectOverview p.ref)

                                        _ ->
                                            Cmd.none

                                DefinitionSearch s ->
                                    -- TODO, support terms vs types vs constructors
                                    case Maybe.andThen SearchResults.focus (Search.searchResults s) of
                                        Just d ->
                                            let
                                                perspective =
                                                    Perspective.relativeRootPerspective

                                                ref =
                                                    Reference.fromFQN Reference.TermReference d.fqn
                                            in
                                            Route.navigate appContext.navKey
                                                (Route.projectBranchDefinition
                                                    d.projectRef
                                                    d.branchRef
                                                    perspective
                                                    ref
                                                )

                                        _ ->
                                            Cmd.none

                                _ ->
                                    Cmd.none
                    in
                    ( newModel, Cmd.batch [ cmd, navCmd ] )

                Sequence _ Tab ->
                    case model.nameSearch of
                        Success q results ->
                            let
                                val =
                                    valueWithFocusedName model.fieldValue results
                            in
                            ( { model
                                | fieldValue = val
                                , nameSearch = Searching q Nothing
                                , search =
                                    toDefinitionSearchSearchingWithQuery model.search val
                              }
                            , Cmd.batch
                                [ searchDefinitions appContext model.filter val
                                , searchNames appContext model.filter val
                                ]
                            )

                        _ ->
                            case model.search of
                                EntitySearch s ->
                                    case Search.searchResultsFocus s of
                                        Just (UserMatch u) ->
                                            ( { model
                                                | nameSearch = NotAsked ""
                                                , filter = UserFilter u.handle
                                                , fieldValue = ""
                                                , search = NoSearch
                                              }
                                            , Cmd.none
                                            )

                                        Just (ProjectMatch p) ->
                                            ( { model
                                                | nameSearch = NotAsked ""
                                                , filter = ProjectFilter p.ref
                                                , fieldValue = ""
                                                , search = NoSearch
                                              }
                                            , Cmd.none
                                            )

                                        _ ->
                                            ( { model | nameSearch = NotAsked "", search = NoSearch }, Cmd.none )

                                _ ->
                                    ( { model | nameSearch = NotAsked "", search = NoSearch }, Cmd.none )

                Sequence _ Backspace ->
                    if String.isEmpty newModel.fieldValue then
                        ( { newModel
                            | filter = NoFilter
                            , nameSearch = NotAsked ""
                            , search = NoSearch
                          }
                        , cmd
                        )

                    else
                        ( newModel, cmd )

                _ ->
                    ( newModel, cmd )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd )



-- UPDATE HELPERS


updateForValue : AppContext -> Model -> String -> ( Model, Cmd Msg )
updateForValue appContext model value =
    let
        hasEnoughChars =
            String.length value > 2
    in
    if String.endsWith "  " value then
        -- Ignore 2 spaces after each other
        ( model, Cmd.none )

    else if value == "@" || String.isEmpty value then
        ( { model
            | fieldValue = value
            , search = NoSearch
            , nameSearch = NotAsked ""
          }
        , Cmd.none
        )

    else if hasEnoughChars && String.startsWith "@" value && String.endsWith " " value then
        let
            ( filter, val, search_ ) =
                case model.search of
                    EntitySearch s ->
                        case Search.searchResultsFocus s of
                            Just (UserMatch u) ->
                                ( UserFilter u.handle, "", NoSearch )

                            Just (ProjectMatch p) ->
                                ( ProjectFilter p.ref, "", NoSearch )

                            _ ->
                                ( NoFilter, value, model.search )

                    _ ->
                        ( NoFilter, value, model.search )
        in
        ( { model
            | fieldValue = val
            , search = search_
            , nameSearch = NotAsked ""
            , filter = filter
          }
        , Cmd.none
        )

    else if hasEnoughChars && String.startsWith "@" value then
        -- "@" is reserved for user and project search
        ( { model
            | fieldValue = value
            , search = toEntitySearchSearchingWithQuery model.search value
            , nameSearch = NotAsked ""
          }
        , searchEntities appContext model.filter value
        )

    else if hasEnoughChars && String.endsWith " " value then
        -- Space will just always clear name search and trigger a definition search.
        -- It might give no results (if the user didn't complete a name suggestion),
        -- but that's fine, they can edit their query after seeing "no results".
        ( { model
            | fieldValue = value
            , search = toDefinitionSearchSearchingWithQuery model.search value
            , nameSearch = NotAsked ""
          }
        , searchDefinitions appContext model.filter value
        )

    else if hasEnoughChars then
        -- search for names by the last segment
        ( { model
            | fieldValue = value
            , nameSearch = Searching value Nothing
            , search = NoSearch
          }
        , searchNames appContext model.filter value
        )

    else
        ( { model | fieldValue = value }, Cmd.none )



-- EFFECTS


searchEntities : AppContext -> Filter -> String -> Cmd Msg
searchEntities appContext _ query =
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
                [ when decodeTag ((==) "User") (Decode.map UserMatch User.decodeDetails)
                , when decodeTag ((==) "Project") (Decode.map ProjectMatch decodeProjectSearchMatch)
                ]
    in
    ShareApi.search query
        |> HttpApi.toRequest
            (Decode.list decodeMatch)
            (\r -> EntitySearchFinished { query = query, results = r })
        |> HttpApi.perform appContext.api


searchDefinitions : AppContext -> Filter -> String -> Cmd Msg
searchDefinitions _ _ query =
    Util.delayMsg 250
        (DefinitionSearchFinished
            { query = query
            , results =
                Ok
                    [ { type_ = TypeMatch
                      , displayName = FQN.fromString "List"
                      , fqn = FQN.fromString "data.List"
                      , projectRef = ProjectRef.unsafeFromString "unison" "base"
                      , branchRef = BranchRef.main_
                      }
                    , { type_ = TermMatch
                      , displayName = FQN.fromString "List.map"
                      , fqn = FQN.fromString "data.List.map"
                      , projectRef = ProjectRef.unsafeFromString "unison" "base"
                      , branchRef = BranchRef.main_
                      }
                    , { type_ = TermMatch
                      , displayName = FQN.fromString "List.foldLeft"
                      , fqn = FQN.fromString "data.List.foldLeft"
                      , projectRef = ProjectRef.unsafeFromString "unison" "base"
                      , branchRef = BranchRef.main_
                      }
                    ]
            }
        )


searchNames : AppContext -> Filter -> String -> Cmd Msg
searchNames _ _ query =
    let
        q =
            query
                |> String.trim
                |> String.split " "
                |> ListE.last
                |> Maybe.withDefault query
    in
    Util.delayMsg 250
        (NameSearchFinished
            { query = q
            , results = Ok [ q ++ "omatic", q ++ "ally", q ++ "tional" ]
            }
        )



-- HELPERS


toEntitySearchSearchingWithQuery : MainSearch -> String -> MainSearch
toEntitySearchSearchingWithQuery search query =
    case search of
        EntitySearch s ->
            EntitySearch (s |> Search.toSearching |> Search.withQuery query)

        _ ->
            EntitySearch (Searching query Nothing)


toDefinitionSearchSearchingWithQuery : MainSearch -> String -> MainSearch
toDefinitionSearchSearchingWithQuery search query =
    case search of
        DefinitionSearch s ->
            DefinitionSearch (s |> Search.toSearching |> Search.withQuery query)

        _ ->
            DefinitionSearch (Searching query Nothing)


valueWithFocusedName : String -> SearchResults.SearchResults String -> String
valueWithFocusedName value results =
    value
        -- Remove the trailing space so we
        -- can better replace the partial
        -- name with the focused name from
        -- the search
        |> String.trim
        |> String.split " "
        |> ListE.init
        |> Maybe.map2
            (\focused xs -> xs ++ [ focused ])
            (SearchResults.focus results)
        |> MaybeE.unwrap value (String.join " ")


suggestedNamePart : Model -> String
suggestedNamePart model =
    let
        suggestedNamePart_ suggested value =
            String.dropLeft (String.length value) suggested
    in
    model.fieldValue
        |> String.split " "
        |> ListE.last
        |> Maybe.map2
            suggestedNamePart_
            (Search.searchResultsFocus model.nameSearch)
        |> Maybe.withDefault ""



-- VIEW


viewField : Model -> Html Msg
viewField model =
    let
        shadowValue =
            if model.hasFocus then
                span [ class "name-suggestion" ]
                    [ span [ class "value" ] [ text model.fieldValue ]
                    , span [ class "shadow-suggestion" ] [ text (suggestedNamePart model) ]
                    ]

            else
                UI.nothing

        ( filterTag, placeholder_ ) =
            case model.filter of
                UserFilter handle ->
                    ( Tag.tag (UserHandle.toString handle)
                        |> Tag.withIcon Icon.user
                        |> Just
                    , "Search projects and definitions"
                    )

                ProjectFilter ref ->
                    ( Tag.tag (ProjectRef.toString ref)
                        |> Tag.withIcon Icon.pencilRuler
                        |> Just
                    , "Search definitions"
                    )

                NoFilter ->
                    ( Nothing, "Search users, projects, and definitions" )
    in
    div [ class "search-field" ]
        [ span [ class "search-icon" ] [ Icon.view Icon.search ]
        , filterTag
            |> Maybe.map
                (Tag.extraLarge
                    >> Tag.withDismissRight (Click.onClick ClearFilter)
                    >> Tag.view
                )
            |> Maybe.withDefault UI.nothing
        , div [ class "inner-field" ]
            [ input
                [ type_ "text"
                , value model.fieldValue
                , onInput UpdateFieldValue
                , onBlur (UpdateFocus False)
                , onFocus (UpdateFocus True)
                , spellcheck False
                , placeholder placeholder_
                ]
                []
            , shadowValue
            ]
        ]


viewMatchKeyboardShortcuts : KeyboardShortcut.Model -> Html msg
viewMatchKeyboardShortcuts keyboardShortcut =
    span [ class "keyboard-shortcuts" ]
        [ text "Go to: "
        , KeyboardShortcut.view
            keyboardShortcut
            (KeyboardShortcut.single Key.Enter)
        , text "Filter by: "
        , KeyboardShortcut.view
            keyboardShortcut
            (KeyboardShortcut.single Key.Tab)
        ]


viewEntityMatch : KeyboardShortcut.Model -> EntityMatch -> Bool -> Html Msg
viewEntityMatch keyboardShortcut match isFocused =
    let
        keyboardShortcuts =
            if isFocused then
                viewMatchKeyboardShortcuts keyboardShortcut

            else
                UI.nothing
    in
    case match of
        UserMatch u ->
            let
                bio =
                    u.bio
                        |> Maybe.map (Markdown.toHtml [])
                        |> Maybe.map (\m -> span [ class "bio" ] [ m ])
                        |> Maybe.withDefault UI.nothing
            in
            div
                [ class "match user-match"
                , classList [ ( "focused", isFocused ) ]
                ]
                [ ProfileSnippet.profileSnippet u
                    |> ProfileSnippet.view
                , bio
                , keyboardShortcuts
                ]

        ProjectMatch p ->
            let
                summary =
                    p.summary
                        |> Maybe.map (\s -> span [ class "summary" ] [ text s ])
                        |> Maybe.withDefault UI.nothing
            in
            div
                [ class "match project-match"
                , classList [ ( "focused", isFocused ) ]
                ]
                [ ProjectListing.projectListing p
                    |> ProjectListing.large
                    |> ProjectListing.view
                , summary
                , keyboardShortcuts
                ]


viewDefinitionMatch : KeyboardShortcut.Model -> DefinitionSearchMatch -> Bool -> Html Msg
viewDefinitionMatch keyboardShortcut def isFocused =
    let
        keyboardShortcuts =
            if isFocused then
                viewMatchKeyboardShortcuts keyboardShortcut

            else
                UI.nothing

        defIcon =
            case def.type_ of
                TermMatch ->
                    Icon.term

                TypeMatch ->
                    Icon.type_

                DataConstructorMatch ->
                    Icon.type_

                AbilityConstructorMatch ->
                    Icon.ability
    in
    div
        [ class "match definition-match"
        , classList [ ( "focused", isFocused ) ]
        ]
        [ span [ class "definition-info" ] [ Icon.view defIcon, FQN.view def.displayName ], keyboardShortcuts ]


viewSearchSheet : (a -> Bool -> Html Msg) -> Search a -> Html Msg
viewSearchSheet viewMatch search =
    let
        viewSheet matches =
            div [ class "main-result-sheet" ]
                (SearchResults.mapToList viewMatch matches)
    in
    case search of
        NotAsked _ ->
            UI.nothing

        Searching _ matches ->
            matches
                |> Maybe.map viewSheet
                |> Maybe.withDefault UI.nothing

        Success _ matches ->
            viewSheet matches

        Failure _ _ ->
            div [ class "main-result-sheet" ] [ text "Error..." ]


viewMainSearch : KeyboardShortcut.Model -> MainSearch -> Html Msg
viewMainSearch keyboardShortcut mainSearch =
    case mainSearch of
        NoSearch ->
            UI.nothing

        EntitySearch s ->
            viewSearchSheet (viewEntityMatch keyboardShortcut) s

        DefinitionSearch s ->
            viewSearchSheet (viewDefinitionMatch keyboardShortcut) s


view : Model -> Html Msg
view model =
    let
        keyboardEvent =
            KeyboardEvent.on KeyboardEvent.Keydown Keydown
                |> KeyboardEvent.stopPropagation
                |> KeyboardEvent.preventDefaultWhen
                    (\evt ->
                        List.member
                            evt.key
                            [ ArrowUp
                            , ArrowDown
                            , Tab
                            , Escape
                            , Enter
                            ]
                    )
                |> KeyboardEvent.attach

        mainSearch =
            if model.hasFocus then
                viewMainSearch model.keyboardShortcut model.search

            else
                UI.nothing
    in
    Html.node "search"
        [ class "omni-search", keyboardEvent ]
        [ viewField model
        , mainSearch
        ]
