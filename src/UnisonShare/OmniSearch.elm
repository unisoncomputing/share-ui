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
   * Triggers if the value starts with a #, then we know its a hash based query

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
import Code.Definition.Term as Term exposing (TermSignature)
import Code.Definition.Type as Type exposing (TypeSource)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Perspective as Perspective
import Code.Syntax as Syntax
import Code.Syntax.SyntaxConfig as SyntaxConfig
import Code.Version as Version
import Html exposing (Html, code, div, h2, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, name, placeholder, spellcheck, tabindex, type_, value)
import Html.Events exposing (onInput)
import Json.Decode as Decode exposing (nullable, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (tag)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search(..))
import Lib.SearchResults as SearchResults
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import List.Extra as ListE
import Maybe.Extra as MaybeE
import Regex
import String.Extra as StringE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Divider as Divider
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut exposing (KeyboardShortcut(..))
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.Modal as Modal
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Org as Org
import UnisonShare.Project as Project
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import UnisonShare.Session as Session
import UnisonShare.User as User



-- MODEL


type alias ProjectSearchMatch =
    Project.Project { summary : Maybe String }


type EntityMatch
    = ProjectMatch ProjectSearchMatch
    | UserMatch User.UserSummary
    | OrgMatch Org.OrgSummary


type DefinitionMatchType
    = TermMatch TermSignature
    | TypeMatch TypeSource
    | DataConstructorMatch TermSignature
    | AbilityConstructorMatch TermSignature


type alias DefinitionSearchMatch =
    { type_ : DefinitionMatchType
    , displayName : FQN
    , fqn : FQN
    , projectRef : ProjectRef
    , branchRef : BranchRef
    }


type BlendedSearchMatch
    = BlendedEntityMatch EntityMatch
    | BlendedDefinitionMatch DefinitionSearchMatch


type MainSearch
    = NoSearch
    | BlendedSearch (Search BlendedSearchMatch)


type Filter
    = NoFilter
    | ProjectFilter ProjectRef
    | UserFilter UserHandle


type OmniSearchModal
    = NoModal
    | SearchHelpModal


type alias Model =
    { fieldValue : String
    , filter : Filter
    , search : MainSearch
    , nameSearch : Search String
    , keyboardShortcut : KeyboardShortcut.Model
    , modal : OmniSearchModal
    }


init : AppContext -> Maybe String -> Maybe String -> ( Model, Cmd Msg )
init appContext query filter =
    let
        fieldValue =
            Maybe.withDefault "" query

        filter_ =
            filter
                |> Maybe.andThen filterFromString
                |> Maybe.withDefault NoFilter

        ( search, cmd ) =
            if String.startsWith "@" fieldValue then
                ( toSearchSearchingWithQuery NoSearch fieldValue
                , Cmd.batch
                    [ searchEntities appContext filter_ fieldValue
                    , searchDefinitions appContext filter_ (withoutAtPrefix fieldValue)
                    ]
                )

            else if fieldValue /= "" then
                ( toSearchSearchingWithQuery NoSearch fieldValue
                , Cmd.batch
                    [ searchEntities appContext filter_ (ensureAtPrefix fieldValue)
                    , searchDefinitions appContext filter_ fieldValue
                    ]
                )

            else
                ( NoSearch, Cmd.none )

        model =
            { fieldValue = fieldValue
            , filter = filter_
            , search = search
            , nameSearch = Search.empty
            , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
            , modal = NoModal
            }
    in
    ( model, cmd )


filterFromString : String -> Maybe Filter
filterFromString s =
    case String.split "/" s of
        [ handle, slug ] ->
            Maybe.map ProjectFilter (ProjectRef.fromString handle slug)

        _ ->
            Maybe.map UserFilter (UserHandle.fromString s)


filterToString : Filter -> String
filterToString filter =
    case filter of
        NoFilter ->
            ""

        UserFilter h ->
            UserHandle.toString h

        ProjectFilter pRef ->
            ProjectRef.toString pRef



-- UPDATE


type Msg
    = UpdateFieldValue String
    | EntitySearchFinished { query : String, results : HttpResult (List EntityMatch) }
    | SearchDefinitions Filter String
    | DefinitionSearchFinished { query : String, results : HttpResult (List DefinitionSearchMatch) }
    | NameSearchFinished { query : String, results : HttpResult (List String) }
    | ClearFilter
    | Keydown KeyboardEvent
    | ShowSearchHelpModal
    | CloseModal
    | SendUpdatedParams { query : String, filter : String }
    | KeyboardShortcutMsg KeyboardShortcut.Msg


type OutMsg
    = NoOut
    | UpdateParams { query : String, filter : String }


update : AppContext -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext msg model =
    let
        updateQuery q filter =
            Util.delayMsg 300 (SendUpdatedParams { query = q, filter = filterToString filter })
    in
    case msg of
        SendUpdatedParams params ->
            if params.query == model.fieldValue && params.filter == filterToString model.filter then
                ( model, Cmd.none, UpdateParams params )

            else
                ( model, Cmd.none, NoOut )

        UpdateFieldValue value ->
            let
                ( model_, cmd ) =
                    updateForValue appContext model value
            in
            ( model_, Cmd.batch [ cmd, updateQuery model_.fieldValue model_.filter ], NoOut )

        EntitySearchFinished res ->
            -- Are we still searching for the same thing?
            if res.query == ensureAtPrefix model.fieldValue then
                let
                    toTake =
                        if isEntityQuery model.fieldValue then
                            8

                        else
                            3
                in
                case ( model.search, res.results ) of
                    ( BlendedSearch (Searching q _), Ok matches ) ->
                        let
                            matches_ =
                                matches
                                    |> List.take toTake
                                    |> List.map BlendedEntityMatch
                                    |> SearchResults.fromList
                        in
                        ( { model | search = BlendedSearch (Success q matches_) }
                        , Cmd.none
                        , NoOut
                        )

                    ( BlendedSearch (Success q prevMatches), Ok matches ) ->
                        let
                            blendedMatches =
                                matches
                                    |> List.take toTake
                                    |> List.map BlendedEntityMatch

                            matches_ =
                                if List.length (String.split " " model.fieldValue) > 1 then
                                    prevMatches

                                else
                                    SearchResults.prepend prevMatches blendedMatches
                                        |> SearchResults.uniqueMatchesBy uniqueMatchReference
                        in
                        ( { model | search = BlendedSearch (Success q matches_) }
                        , Cmd.none
                        , NoOut
                        )

                    ( BlendedSearch (Searching q _), Err e ) ->
                        ( { model | search = BlendedSearch (Failure q e) }, Cmd.none, NoOut )

                    _ ->
                        ( model, Cmd.none, NoOut )

            else
                ( model, Cmd.none, NoOut )

        SearchDefinitions filter query ->
            ( { model
                | search = toSearchSearchingWithQuery model.search query
              }
            , Cmd.batch
                [ searchEntities appContext filter (ensureAtPrefix query)
                , searchDefinitions appContext filter query
                ]
            , NoOut
            )

        DefinitionSearchFinished res ->
            let
                val =
                    case model.nameSearch of
                        Success _ results ->
                            valueWithFocusedName model.fieldValue results

                        _ ->
                            model.fieldValue

                toTake =
                    if not (isEntityQuery model.fieldValue) then
                        case model.search of
                            BlendedSearch s ->
                                case Search.length s of
                                    Just n ->
                                        8 - n

                                    Nothing ->
                                        8

                            _ ->
                                8

                    else
                        3
            in
            -- Are we still searching for the same thing?
            if res.query == val || res.query == withoutAtPrefix model.fieldValue then
                case ( model.search, res.results ) of
                    ( BlendedSearch (Searching q _), Ok matches ) ->
                        let
                            matches_ =
                                matches
                                    |> List.take toTake
                                    |> List.map BlendedDefinitionMatch
                                    |> SearchResults.fromList
                        in
                        ( { model | search = BlendedSearch (Success q matches_) }
                        , Cmd.none
                        , NoOut
                        )

                    ( BlendedSearch (Success q prevMatches), Ok matches ) ->
                        let
                            blendedMatches =
                                matches
                                    |> List.take toTake
                                    |> List.map BlendedDefinitionMatch

                            matches_ =
                                SearchResults.append prevMatches blendedMatches
                                    |> SearchResults.uniqueMatchesBy uniqueMatchReference
                        in
                        ( { model | search = BlendedSearch (Success q matches_) }
                        , Cmd.none
                        , NoOut
                        )

                    ( BlendedSearch (Searching q _), Err e ) ->
                        ( { model | search = BlendedSearch (Failure q e) }, Cmd.none, NoOut )

                    _ ->
                        ( model, Cmd.none, NoOut )

            else
                ( model, Cmd.none, NoOut )

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
                                            ( toSearchSearchingWithQuery model.search q_
                                            , Cmd.batch
                                                -- We don't want to search entities by the autocompleted name, instead we search on the actual value
                                                [ searchEntities appContext model.filter (ensureAtPrefix model.fieldValue)
                                                , searchDefinitions appContext model.filter q_
                                                ]
                                            )
                                        )
                                    |> Maybe.withDefault
                                        ( toSearchSearchingWithQuery model.search model.fieldValue
                                        , Cmd.batch
                                            [ searchEntities appContext model.filter (ensureAtPrefix model.fieldValue)
                                            , searchDefinitions appContext model.filter model.fieldValue
                                            ]
                                        )
                        in
                        ( { model | nameSearch = Success q results, search = search }, cmd, NoOut )

                    ( Searching q _, Err e ) ->
                        ( { model | nameSearch = Failure q e }, Cmd.none, NoOut )

                    _ ->
                        ( model, Cmd.none, NoOut )

            else
                ( model, Cmd.none, NoOut )

        ClearFilter ->
            let
                ( model_, cmd ) =
                    updateForValue appContext { model | filter = NoFilter } model.fieldValue
            in
            ( model_, Cmd.batch [ cmd, updateQuery model_.fieldValue model_.filter ], NoOut )

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
                    let
                        model_ =
                            { newModel | search = NoSearch, fieldValue = "", nameSearch = Search.empty }
                    in
                    ( model_, Cmd.batch [ cmd, updateQuery model_.fieldValue model_.filter ], NoOut )

                Sequence _ ArrowUp ->
                    case model.search of
                        BlendedSearch s ->
                            ( { newModel | search = BlendedSearch (Search.searchResultsPrev s) }, cmd, NoOut )

                        _ ->
                            ( newModel, cmd, NoOut )

                Sequence _ ArrowDown ->
                    case model.search of
                        BlendedSearch s ->
                            ( { newModel | search = BlendedSearch (Search.searchResultsNext s) }, cmd, NoOut )

                        _ ->
                            ( newModel, cmd, NoOut )

                Sequence _ Enter ->
                    let
                        navCmd =
                            case model.search of
                                BlendedSearch s ->
                                    case Maybe.andThen SearchResults.focus (Search.searchResults s) of
                                        Just (BlendedEntityMatch (UserMatch u)) ->
                                            Route.navigate appContext.navKey (Route.userProfile u.handle)

                                        Just (BlendedEntityMatch (ProjectMatch p)) ->
                                            Route.navigate appContext.navKey (Route.projectOverview p.ref)

                                        Just (BlendedDefinitionMatch d) ->
                                            let
                                                perspective =
                                                    Perspective.relativeRootPerspective

                                                ref =
                                                    definitionSearchMatchToReference d
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
                    ( newModel, Cmd.batch [ cmd, navCmd ], NoOut )

                Sequence _ Tab ->
                    case model.nameSearch of
                        Success q results ->
                            let
                                val =
                                    valueWithFocusedName model.fieldValue results

                                ( nameSearch, searchNamesCmd ) =
                                    case searchNames appContext model.filter val of
                                        Nothing ->
                                            ( Search.empty, Cmd.none )

                                        Just searchNamesCmd_ ->
                                            ( Searching q Nothing, searchNamesCmd_ )

                                model_ =
                                    { model
                                        | fieldValue = val
                                        , nameSearch = nameSearch
                                        , search =
                                            toSearchSearchingWithQuery model.search val
                                    }
                            in
                            ( model_
                            , Cmd.batch
                                [ searchDefinitions appContext model.filter val
                                , searchEntities appContext model.filter (ensureAtPrefix val)
                                , searchNamesCmd
                                , updateQuery model_.fieldValue model_.filter
                                ]
                            , NoOut
                            )

                        _ ->
                            case model.search of
                                BlendedSearch s ->
                                    case Search.searchResultsFocus s of
                                        Just (BlendedEntityMatch (UserMatch u)) ->
                                            let
                                                model_ =
                                                    { model
                                                        | nameSearch = Search.empty
                                                        , filter = UserFilter u.handle
                                                        , fieldValue = ""
                                                        , search = NoSearch
                                                    }
                                            in
                                            ( model_
                                            , updateQuery model_.fieldValue model_.filter
                                            , NoOut
                                            )

                                        Just (BlendedEntityMatch (ProjectMatch p)) ->
                                            let
                                                model_ =
                                                    { model
                                                        | nameSearch = Search.empty
                                                        , filter = ProjectFilter p.ref
                                                        , fieldValue = ""
                                                        , search = NoSearch
                                                    }
                                            in
                                            ( model_
                                            , updateQuery model_.fieldValue model_.filter
                                            , NoOut
                                            )

                                        _ ->
                                            ( { model | nameSearch = Search.empty, search = NoSearch }, Cmd.none, NoOut )

                                _ ->
                                    ( { model | nameSearch = Search.empty, search = NoSearch }, Cmd.none, NoOut )

                Sequence _ Backspace ->
                    if String.isEmpty newModel.fieldValue then
                        let
                            model_ =
                                { newModel
                                    | filter = NoFilter
                                    , nameSearch = Search.empty
                                    , search = NoSearch
                                }
                        in
                        ( model_
                        , Cmd.batch
                            [ cmd
                            , updateQuery model_.fieldValue model_.filter
                            ]
                        , NoOut
                        )

                    else
                        ( newModel, cmd, NoOut )

                _ ->
                    ( newModel, cmd, NoOut )

        ShowSearchHelpModal ->
            ( { model | modal = SearchHelpModal }, Cmd.none, NoOut )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, cmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg cmd, NoOut )



-- UPDATE HELPERS


uniqueMatchReference : BlendedSearchMatch -> String
uniqueMatchReference match =
    case match of
        BlendedEntityMatch (ProjectMatch { ref }) ->
            ProjectRef.toString ref

        BlendedEntityMatch (UserMatch { handle }) ->
            UserHandle.toString handle

        BlendedEntityMatch (OrgMatch { handle }) ->
            UserHandle.toString handle

        BlendedDefinitionMatch { fqn, projectRef, branchRef } ->
            FQN.toString fqn ++ "_" ++ ProjectRef.toString projectRef ++ "_" ++ BranchRef.toString branchRef


isEntityMatch : BlendedSearchMatch -> Bool
isEntityMatch match =
    case match of
        BlendedEntityMatch _ ->
            True

        _ ->
            False


isDefinitionMatch : BlendedSearchMatch -> Bool
isDefinitionMatch match =
    case match of
        BlendedDefinitionMatch _ ->
            True

        _ ->
            False


hasAnyEntityMatches : SearchResults.SearchResults BlendedSearchMatch -> Bool
hasAnyEntityMatches results =
    results
        |> SearchResults.toList
        |> List.any isEntityMatch


isOnlyDefinitionMatches : SearchResults.SearchResults BlendedSearchMatch -> Bool
isOnlyDefinitionMatches results =
    results
        |> SearchResults.toList
        |> List.all isEntityMatch


isOnlyEntityMatches : SearchResults.SearchResults BlendedSearchMatch -> Bool
isOnlyEntityMatches results =
    results
        |> SearchResults.toList
        |> List.all isEntityMatch


hasAnyDefinitionMatches : SearchResults.SearchResults BlendedSearchMatch -> Bool
hasAnyDefinitionMatches results =
    results
        |> SearchResults.toList
        |> List.any isEntityMatch


isEntityQuery : String -> Bool
isEntityQuery val =
    String.startsWith "@" val


ensureAtPrefix : String -> String
ensureAtPrefix val =
    if String.startsWith "@" val then
        val

    else
        "@" ++ val


withoutAtPrefix : String -> String
withoutAtPrefix val =
    if String.startsWith "@" val then
        String.dropLeft 1 val

    else
        val


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
            , nameSearch = Search.empty
          }
        , Cmd.none
        )

    else if hasEnoughChars && String.startsWith "@" value && String.endsWith " " value then
        let
            ( filter, val, search_ ) =
                case model.search of
                    BlendedSearch s ->
                        case Search.searchResultsFocus s of
                            Just (BlendedEntityMatch (UserMatch u)) ->
                                ( UserFilter u.handle, "", NoSearch )

                            Just (BlendedEntityMatch (ProjectMatch p)) ->
                                ( ProjectFilter p.ref, "", NoSearch )

                            _ ->
                                ( NoFilter, value, model.search )

                    _ ->
                        ( NoFilter, value, model.search )
        in
        ( { model
            | fieldValue = val
            , search = search_
            , nameSearch = Search.empty
            , filter = filter
          }
        , Cmd.none
        )

    else if hasEnoughChars && String.startsWith "@" value then
        -- "@" is reserved for user and project search
        ( { model
            | fieldValue = value
            , search = toSearchSearchingWithQuery model.search value
            , nameSearch = Search.empty
          }
        , Cmd.batch
            [ searchEntities appContext model.filter value
            , searchDefinitions appContext model.filter (withoutAtPrefix value)
            ]
        )

    else if hasEnoughChars && String.startsWith "#" value then
        -- "#" is used for hash based definition search
        ( { model
            | fieldValue = value
            , nameSearch = Search.empty
          }
        , Search.debounce (SearchDefinitions model.filter value)
        )

    else if hasEnoughChars && String.endsWith " " value then
        -- Space will just always clear name search and trigger a definition search.
        -- It might give no results (if the user didn't complete a name suggestion),
        -- but that's fine, they can edit their query after seeing "no results".
        ( { model
            | fieldValue = value
            , search = toSearchSearchingWithQuery model.search value
            , nameSearch = Search.empty
          }
        , Cmd.batch
            [ searchEntities appContext model.filter (ensureAtPrefix value)
            , searchDefinitions appContext model.filter value
            ]
        )

    else if hasEnoughChars then
        let
            ( mainSearch, nameSearch, cmd ) =
                case searchNames appContext model.filter value of
                    Nothing ->
                        ( toSearchSearchingWithQuery model.search value
                        , model.nameSearch
                        , Cmd.batch
                            [ searchEntities appContext model.filter (ensureAtPrefix value)
                            , searchDefinitions appContext model.filter value
                            ]
                        )

                    Just searchNamesCmd_ ->
                        ( model.search, Searching value Nothing, searchNamesCmd_ )
        in
        ( { model | fieldValue = value, search = mainSearch, nameSearch = nameSearch }, cmd )

    else
        ( { model | fieldValue = value }, Cmd.none )


isCapitalized : String -> Bool
isCapitalized s =
    s
        |> String.toList
        |> List.head
        |> Maybe.map Char.isUpper
        |> Maybe.withDefault False



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
                [ when tag ((==) "user") (Decode.map UserMatch User.decodeSummary)
                , when tag ((==) "org") (Decode.map OrgMatch Org.decodeSummary)
                , when tag ((==) "project") (Decode.map ProjectMatch decodeProjectSearchMatch)
                ]
    in
    ShareApi.search query
        |> HttpApi.toRequest
            (Decode.list decodeMatch)
            (\r -> EntitySearchFinished { query = query, results = r })
        |> HttpApi.perform appContext.api


searchDefinitions : AppContext -> Filter -> String -> Cmd Msg
searchDefinitions appContext filter query =
    let
        filter_ =
            case filter of
                NoFilter ->
                    Nothing

                ProjectFilter projectRef ->
                    Just ( "project-filter", ProjectRef.toString projectRef )

                UserFilter userHandle ->
                    Just ( "user-filter", UserHandle.toString userHandle )

        decodeKind =
            Decode.field "kind" Decode.string

        decodeDefinition =
            Decode.oneOf
                [ when decodeKind
                    ((==) "term")
                    (Decode.succeed TermMatch
                        |> required "definition" (Term.decodeSignature [ "summary", "contents" ])
                    )
                , when decodeKind
                    ((==) "type")
                    (Decode.succeed TypeMatch
                        |> required "definition" (Type.decodeTypeSource [ "summary", "tag" ] [ "summary", "contents" ])
                    )
                ]

        decode =
            Decode.succeed DefinitionSearchMatch
                |> requiredAt [] decodeDefinition
                |> requiredAt [ "definition", "displayName" ] FQN.decode
                |> required "fqn" FQN.decode
                |> required "projectRef" ProjectRef.decode
                |> required "branchRef" BranchRef.decode
    in
    ShareApi.searchDefinitions filter_ query
        |> HttpApi.toRequest
            (Decode.field "results" (Decode.list decode))
            (\r -> DefinitionSearchFinished { query = query, results = r })
        |> HttpApi.perform appContext.api


searchNames : AppContext -> Filter -> String -> Maybe (Cmd Msg)
searchNames appContext filter query =
    let
        params =
            case filter of
                NoFilter ->
                    []

                ProjectFilter projectRef ->
                    [ ( "project-filter", ProjectRef.toString projectRef ) ]

                UserFilter userHandle ->
                    [ ( "user-filter", UserHandle.toString userHandle ) ]

        regex =
            Maybe.withDefault Regex.never <|
                Regex.fromString "[^a-zA-Z .]+"

        perform q =
            ShareApi.searchNames params q
                |> HttpApi.toRequest
                    (Decode.list (Decode.field "token" Decode.string))
                    (\r -> NameSearchFinished { query = query, results = r })
                |> HttpApi.perform appContext.api

        skipSpecialChars word =
            if Regex.contains regex word then
                Nothing

            else
                Just word

        hasEnoughChars s =
            if String.length s > 2 then
                Just s

            else
                Nothing
    in
    query
        |> StringE.clean
        |> String.split " "
        |> ListE.last
        |> Maybe.andThen skipSpecialChars
        |> Maybe.andThen hasEnoughChars
        |> Maybe.map perform



-- HELPERS


isMainSearchSearching : MainSearch -> Bool
isMainSearchSearching search =
    case search of
        BlendedSearch s ->
            Search.isSearching s

        _ ->
            False


toSearchSearchingWithQuery : MainSearch -> String -> MainSearch
toSearchSearchingWithQuery search query =
    case search of
        BlendedSearch s ->
            BlendedSearch (s |> Search.toSearching |> Search.withQuery query)

        _ ->
            BlendedSearch (Searching query Nothing)


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


viewField : Model -> Bool -> Html Msg
viewField model isSearching =
    let
        shadowValue =
            span [ class "name-suggestion" ]
                [ span [ class "value" ] [ text model.fieldValue ]
                , span [ class "shadow-suggestion" ] [ text (suggestedNamePart model) ]
                ]

        ( filterTag, placeholder_ ) =
            case model.filter of
                UserFilter handle ->
                    ( Tag.tag (UserHandle.toString handle)
                        |> Tag.withIcon Icon.user
                        |> Just
                    , "Ex: \"@unison/base\" or \"Map Boolean\""
                    )

                ProjectFilter ref ->
                    ( Tag.tag (ProjectRef.toString ref)
                        |> Tag.withIcon Icon.pencilRuler
                        |> Just
                    , "Ex: \"List.map\" or \"Map Boolean\""
                    )

                NoFilter ->
                    ( Nothing, "Ex: \"@unison\", \"@unison/base\", or \"Map Boolean\"" )
    in
    div [ class "search-field" ]
        [ span [ class "search-icon" ]
            [ Icon.search |> Icon.withToggleAnimation isSearching |> Icon.view
            ]
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
                , name "search"
                , value model.fieldValue
                , onInput UpdateFieldValue
                , autocomplete False
                , spellcheck False
                , placeholder placeholder_
                , autofocus True
                ]
                []
            , shadowValue
            , Button.icon ShowSearchHelpModal Icon.questionmark
                |> Button.subdued
                |> Button.view
            ]
        ]


viewMatchKeyboardShortcuts : KeyboardShortcut.Model -> Bool -> Html msg
viewMatchKeyboardShortcuts keyboardShortcut includeFilter =
    let
        filter =
            if includeFilter then
                span [ class "filter-by" ]
                    [ text "Filter by "
                    , KeyboardShortcut.view
                        keyboardShortcut
                        (KeyboardShortcut.single Key.Tab)
                    ]

            else
                UI.nothing
    in
    div [ class "keyboard-shortcuts" ]
        [ span [ class "go-to" ]
            [ text "Go to "
            , KeyboardShortcut.view
                keyboardShortcut
                (KeyboardShortcut.single Key.Enter)
            ]
        , filter
        ]


viewBlendedMatch : KeyboardShortcut.Model -> BlendedSearchMatch -> Bool -> Html Msg
viewBlendedMatch keyboardShortcut match isFocused =
    case match of
        BlendedEntityMatch entityMatch ->
            viewEntityMatch keyboardShortcut entityMatch isFocused

        BlendedDefinitionMatch defMatch ->
            viewDefinitionMatch keyboardShortcut defMatch isFocused


viewEntityMatch : KeyboardShortcut.Model -> EntityMatch -> Bool -> Html Msg
viewEntityMatch keyboardShortcut match isFocused =
    let
        keyboardShortcuts =
            if isFocused then
                viewMatchKeyboardShortcuts keyboardShortcut True

            else
                UI.nothing
    in
    case match of
        UserMatch u ->
            Click.view
                [ class "match user-match"
                , classList [ ( "focused", isFocused ) ]
                ]
                [ ProfileSnippet.profileSnippet u
                    |> ProfileSnippet.view
                , keyboardShortcuts
                ]
                (Link.userProfile u.handle)

        OrgMatch o ->
            Click.view
                [ class "match org-match"
                , classList [ ( "focused", isFocused ) ]
                ]
                [ ProfileSnippet.profileSnippet o
                    |> ProfileSnippet.view
                , keyboardShortcuts
                ]
                (Link.orgProfile o.handle)

        ProjectMatch p ->
            let
                summary =
                    p.summary
                        |> Maybe.map (\s -> span [ class "summary" ] [ text s ])
                        |> Maybe.withDefault UI.nothing
            in
            Click.view
                [ class "match project-match"
                , classList [ ( "focused", isFocused ) ]
                ]
                [ div [ class "info-and-summary" ]
                    [ div [ class "project-info" ]
                        [ ProjectListing.projectListing p
                            |> ProjectListing.large
                            |> ProjectListing.view
                        ]
                    , summary
                    ]
                , keyboardShortcuts
                ]
                (Link.projectOverview p.ref)


definitionSearchMatchToReference : DefinitionSearchMatch -> Reference.Reference
definitionSearchMatchToReference d =
    case d.type_ of
        TermMatch _ ->
            Reference.fromFQN Reference.TermReference d.fqn

        TypeMatch _ ->
            Reference.fromFQN Reference.TypeReference d.fqn

        DataConstructorMatch _ ->
            Reference.fromFQN Reference.DataConstructorReference d.fqn

        AbilityConstructorMatch _ ->
            Reference.fromFQN Reference.AbilityConstructorReference d.fqn


viewDefinitionMatch : KeyboardShortcut.Model -> DefinitionSearchMatch -> Bool -> Html Msg
viewDefinitionMatch keyboardShortcut def isFocused =
    let
        keyboardShortcuts =
            if isFocused then
                viewMatchKeyboardShortcuts keyboardShortcut False

            else
                UI.nothing

        ( defIcon, summary ) =
            case def.type_ of
                TermMatch sig ->
                    ( Icon.term, Syntax.view SyntaxConfig.empty (Term.termSignatureSyntax sig) )

                TypeMatch sum ->
                    ( Icon.type_
                    , sum
                        |> Type.typeSourceSyntax
                        |> Maybe.map (Syntax.view SyntaxConfig.empty)
                        |> Maybe.withDefault UI.nothing
                    )

                DataConstructorMatch sig ->
                    ( Icon.dataConstructor, Syntax.view SyntaxConfig.empty (Term.termSignatureSyntax sig) )

                AbilityConstructorMatch sig ->
                    ( Icon.abilityConstructor, Syntax.view SyntaxConfig.empty (Term.termSignatureSyntax sig) )

        shouldTruncateName =
            def.displayName
                |> FQN.toString
                |> String.length
                |> (\len -> len > 19)
    in
    Click.view
        [ class "match definition-match"
        , classList [ ( "focused", isFocused ) ]
        ]
        [ div [ class "info-and-summary" ]
            [ div [ class "definition-info-and-project" ]
                [ span [ class "definition-info", classList [ ( "truncate-name", shouldTruncateName ) ] ]
                    [ div [ class "definition-kind" ] [ Icon.view defIcon ]
                    , span [ class "definition-name" ] [ FQN.view def.displayName ]
                    ]
                , span [ class "project-and-release" ]
                    [ span [ class "project" ]
                        [ text (ProjectRef.toString def.projectRef) ]
                    , def.branchRef
                        |> BranchRef.version
                        |> Maybe.map Version.toString
                        |> Maybe.map (\v -> span [ class "release" ] [ text (" v" ++ v) ])
                        |> Maybe.withDefault UI.nothing
                    ]
                ]
            , code [ class "syntax monochrome" ] [ summary ]
            ]
        , keyboardShortcuts
        ]
        (Link.projectBranchDefinition
            def.projectRef
            def.branchRef
            (definitionSearchMatchToReference def)
        )


viewSearchSheet : AppContext -> (a -> Bool -> Html Msg) -> Search a -> Html Msg
viewSearchSheet appContext viewMatch search =
    let
        viewSheet matches =
            if SearchResults.isEmpty matches then
                div
                    [ class "main-result-sheet empty-state" ]
                    [ text "No matches found" ]

            else
                div [ class "main-result-sheet" ]
                    (matches |> SearchResults.mapToList viewMatch)
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

        Failure _ err ->
            if Session.isSuperAdmin appContext.session then
                div [ class "main-result-sheet" ] [ text (Util.httpErrorToString err) ]

            else
                -- TODO: Improve the quality of this error messaging
                div [ class "main-result-sheet" ]
                    [ StatusBanner.bad "Unexpected error occurred while searching"
                    ]


viewMainSearch : AppContext -> KeyboardShortcut.Model -> MainSearch -> Html Msg
viewMainSearch appContext keyboardShortcut mainSearch =
    case mainSearch of
        NoSearch ->
            UI.nothing

        BlendedSearch s ->
            viewSearchSheet appContext (viewBlendedMatch keyboardShortcut) s


viewSearchHelpModal : AppContext -> Html Msg
viewSearchHelpModal appContext =
    let
        inlineCode t =
            UI.inlineCode [] (text t)

        tabKey =
            KeyboardShortcut.viewBase
                [ KeyboardShortcut.viewKey appContext.operatingSystem Key.Tab False
                ]

        enterKey =
            KeyboardShortcut.viewBase
                [ KeyboardShortcut.viewKey appContext.operatingSystem
                    Key.Enter
                    False
                ]

        upKey =
            KeyboardShortcut.viewBase
                [ KeyboardShortcut.viewKey appContext.operatingSystem
                    Key.ArrowUp
                    False
                ]

        downKey =
            KeyboardShortcut.viewBase
                [ KeyboardShortcut.viewKey appContext.operatingSystem
                    Key.ArrowDown
                    False
                ]

        divider =
            Divider.divider
                |> Divider.small
                |> Divider.withoutMargin
                |> Divider.view

        content =
            Modal.Content
                (div
                    [ class "help-content" ]
                    [ div []
                        [ div [] [ text "Find users, projects, and definitions with the below syntax." ]
                        , div [] [ text "Navigate the result with the ", downKey, text " and ", upKey, text " keys. Select a match with ", enterKey, text " (or click)." ]
                        ]
                    , divider
                    , div []
                        [ h2
                            []
                            [ text "Users and Projects with a ", inlineCode "@", text " prefix" ]
                        , table []
                            [ thead [] [ tr [] [ th [] [ text "Example query" ], th [] [ text "Result" ] ] ]
                            , tbody []
                                [ tr []
                                    [ td [] [ inlineCode "@unison" ]
                                    , td [] [ text "The Unison user." ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "@unison/base", text " or just ", inlineCode "@base" ]
                                    , td [] [ text "The Unison base library." ]
                                    ]
                                ]
                            ]
                        ]
                    , div []
                        [ h2 [] [ text "Definitions by name, type, or hash" ]
                        , table []
                            [ tbody []
                                [ tr []
                                    [ td [] [ inlineCode "List.map", text " or ", inlineCode "Optional" ]
                                    , td [] [ text "Functions and types by name." ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "Map Nat Boolean" ]
                                    , td [] [ text "Functions that mention the types ", inlineCode "Map", text ", ", inlineCode "Nat", text ", and ", inlineCode "Boolean", text "." ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "Map Text a -> a" ]
                                    , td [] [ text "Functions by a mix of concrete and type variables." ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "[a] -> {Abort} a" ]
                                    , td [] [ text "Functions by signature." ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "#asdf1234" ]
                                    , td [] [ text "Definitions by hash." ]
                                    ]
                                ]
                            ]
                        ]
                    , div []
                        [ h2 [] [ text "Filtering by user or project with the ", tabKey, text " key" ]
                        , table []
                            [ tbody []
                                [ tr []
                                    [ td [] [ inlineCode "@unison", text " + ", tabKey ]
                                    , td []
                                        [ text "Filter subsequent searches by the Unison user."
                                        ]
                                    ]
                                , tr
                                    []
                                    [ td [] [ inlineCode "@unison/base", text " + ", tabKey ]
                                    , td [] [ text "Filter subsequent searches by the Unison base project." ]
                                    ]
                                ]
                            ]
                        ]
                    , divider
                    ]
                )
    in
    Modal.modal "omni-search-help-modal" CloseModal content
        |> Modal.withHeader "Searching across Unison Share"
        |> Modal.withActions [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" ]
        |> Modal.view


view : AppContext -> Model -> ( Html Msg, Maybe (Html Msg) )
view appContext model =
    let
        modal =
            case model.modal of
                SearchHelpModal ->
                    Just (viewSearchHelpModal appContext)

                _ ->
                    Nothing

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

        isSearching =
            isMainSearchSearching model.search
    in
    ( Html.node "search"
        [ class "omni-search", tabindex 0, classList [ ( "searching", isSearching ) ], keyboardEvent ]
        [ viewField model isSearching
        , viewMainSearch appContext model.keyboardShortcut model.search
        ]
    , modal
    )
