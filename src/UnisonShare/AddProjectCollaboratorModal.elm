module UnisonShare.AddProjectCollaboratorModal exposing (..)

import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class, classList)
import Http
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search(..))
import Lib.SearchResults as SearchResults
import Lib.Util exposing (decodeTag)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.Click as Click
import UI.Form.RadioField as RadioField
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectAccess as ProjectAccess exposing (ProjectAccess(..))
import UnisonShare.ProjectCollaborator exposing (ProjectCollaborator)
import UnisonShare.Session as Session
import UnisonShare.User as User exposing (UserSummaryWithId)


type Model
    = FindUser (Search UserSummaryWithId)
    | GiveAccess ProjectCollaborator
    | Saving ProjectCollaborator
    | Failure Http.Error ProjectCollaborator
    | Success ProjectCollaborator


init : Model
init =
    FindUser Search.empty



-- UPDATE


type Msg
    = CloseModal
    | UpdateQuery String
    | PerformSearch String
    | FetchUsersFinished String (HttpResult (List UserSummaryWithId))
    | SelectUser UserSummaryWithId
    | SetAccess ProjectAccess
    | AddCollaborator
    | AddCollaboratorFinished (HttpResult ())
    | BackToFindUser


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | AddedCollaborator ProjectCollaborator


update : AppContext -> ProjectRef -> List ProjectCollaborator -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef currentCollaborators msg model =
    case msg of
        CloseModal ->
            ( model, Cmd.none, RequestCloseModal )

        UpdateQuery q ->
            case model of
                FindUser s ->
                    let
                        search =
                            Search.withQuery q s

                        cmd =
                            if Search.hasSubstantialQuery s then
                                Search.debounce (PerformSearch q)

                            else
                                Cmd.none
                    in
                    ( FindUser search, cmd, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        PerformSearch q ->
            case model of
                FindUser s ->
                    if Search.queryEquals q s && Search.hasSubstantialQuery s then
                        ( FindUser (Search.toSearching s), fetchUsers appContext q, NoOutMsg )

                    else
                        ( FindUser (Search.search (Search.query s)), Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        FetchUsersFinished q users ->
            case ( appContext.session, model, users ) of
                ( Session.SignedIn account, FindUser search, Ok users_ ) ->
                    let
                        usersToIgnore =
                            account.handle :: List.map (.user >> .handle) currentCollaborators

                        model_ =
                            if Search.queryEquals q search then
                                users_
                                    |> List.filter (\u -> not (List.member u.handle usersToIgnore))
                                    |> SearchResults.fromList
                                    |> (\res -> Search.toSuccess res search)
                                    |> FindUser

                            else
                                model
                    in
                    ( model_, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        SelectUser user ->
            ( GiveAccess { user = user, access = Viewer }, Cmd.none, NoOutMsg )

        SetAccess access ->
            case model of
                GiveAccess { user } ->
                    ( GiveAccess { user = user, access = access }, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        AddCollaborator ->
            case model of
                GiveAccess collab ->
                    ( Saving collab, addCollaborator appContext projectRef collab, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        AddCollaboratorFinished res ->
            case ( model, res ) of
                ( Saving collab, Ok _ ) ->
                    ( Success collab, Cmd.none, AddedCollaborator collab )

                ( Saving collab, Err e ) ->
                    ( Failure e collab, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        BackToFindUser ->
            ( FindUser Search.empty, Cmd.none, NoOutMsg )



-- EFFECTS


fetchUsers : AppContext -> String -> Cmd Msg
fetchUsers appContext query =
    let
        query_ =
            if String.startsWith "@" query then
                query

            else
                "@" ++ query

        decodeMatch =
            Decode.oneOf
                [ when decodeTag ((==) "User") (Decode.map Just User.decodeSummaryWithId)
                , Decode.succeed Nothing
                ]
    in
    ShareApi.search query_
        |> HttpApi.toRequest (Decode.list decodeMatch) (Result.map MaybeE.values >> FetchUsersFinished query)
        |> HttpApi.perform appContext.api


addCollaborator : AppContext -> ProjectRef -> ProjectCollaborator -> Cmd Msg
addCollaborator appContext projectRef collab =
    ShareApi.createProjectMaintainers projectRef [ collab ]
        |> HttpApi.toRequestWithEmptyResponse AddCollaboratorFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewUser : UserSummaryWithId -> Html msg
viewUser user =
    ProfileSnippet.profileSnippet user |> ProfileSnippet.view


view : Model -> Html Msg
view model =
    let
        modal_ c =
            Modal.content c
                |> Modal.modal "add-project-collaborator-modal" CloseModal
                |> Modal.withHeader "Add collaborator"

        modal =
            case model of
                FindUser search ->
                    let
                        viewMatch user hasFocus =
                            Click.onClick (SelectUser user)
                                |> Click.view
                                    [ class "user-match", classList [ ( "user-match_focus", hasFocus ) ] ]
                                    [ viewUser user
                                    ]

                        searchSheet =
                            case search of
                                Search.NotAsked _ ->
                                    UI.nothing

                                Search.Searching _ _ ->
                                    UI.nothing

                                Search.Success _ results ->
                                    case results of
                                        SearchResults.Empty ->
                                            div [ class "search-sheet search-sheet_empty-state" ]
                                                [ text "Couldn't find anyone matching the search query" ]

                                        _ ->
                                            div [ class "search-sheet" ]
                                                (SearchResults.mapToList viewMatch results)

                                Search.Failure _ _ ->
                                    StatusBanner.bad "An error occurred"
                    in
                    modal_
                        (div []
                            [ TextField.field_ UpdateQuery Nothing (Just "Search people") (Search.query search)
                                |> TextField.withIcon Icon.search
                                |> TextField.withAutofocus
                                |> TextField.view
                            , searchSheet
                            ]
                        )

                GiveAccess { user, access } ->
                    let
                        options =
                            NEL.singleton (RadioField.option "Admin" "Full access, including sensitive and destructive actions." Admin)
                                |> NEL.cons (RadioField.option "Maintain" "Read, download, with merge and write access." Maintainer)
                                |> NEL.cons (RadioField.option "View" "Read, download project. Nothing else." Viewer)
                    in
                    modal_
                        (div [ class "give-access" ]
                            [ div [ class "selected-user" ]
                                [ viewUser user
                                , Button.icon BackToFindUser Icon.trash
                                    |> Button.small
                                    |> Button.subdued
                                    |> Button.view
                                ]
                            , h3 [] [ text "Select a role" ]
                            , RadioField.field "Select access" SetAccess options access
                                |> RadioField.view
                            ]
                        )
                        |> Modal.withActions
                            [ Button.button CloseModal "Cancel"
                                |> Button.subdued
                            , Button.button AddCollaborator ("Add " ++ User.name user)
                                |> Button.emphasized
                            ]

                Saving collab ->
                    modal_
                        (div []
                            [ text
                                ("Adding "
                                    ++ User.name collab.user
                                    ++ " with "
                                    ++ ProjectAccess.toString collab.access
                                    ++ " access"
                                )
                            ]
                        )

                Failure _ collab ->
                    modal_
                        (div []
                            [ text
                                ("Failed to add "
                                    ++ User.name collab.user
                                    ++ " with "
                                    ++ ProjectAccess.toString collab.access
                                    ++ " access"
                                )
                            ]
                        )

                Success collab ->
                    modal_
                        (StatusBanner.good
                            ("Successfully added "
                                ++ User.name collab.user
                                ++ " with "
                                ++ ProjectAccess.toString collab.access
                                ++ " access"
                            )
                        )
    in
    Modal.view modal
