module UnisonShare.AddOrgMemberModal exposing (..)

import Html exposing (Html, div, h3, text)
import Html.Attributes exposing (class, classList)
import Http
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Lib.Decode.Helpers exposing (tag)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Search as Search exposing (Search)
import Lib.SearchResults as SearchResults
import Lib.UserHandle exposing (UserHandle)
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
import UnisonShare.OrgMember as OrgMember exposing (OrgMember)
import UnisonShare.OrgRole as OrgRole exposing (OrgRole(..))
import UnisonShare.Session as Session
import UnisonShare.User as User exposing (UserSummaryWithId)


type alias PotentialOrgMember =
    { user : UserSummaryWithId, role : OrgRole }


type Model
    = FindUser (Search UserSummaryWithId)
    | AssignRole PotentialOrgMember
    | Saving PotentialOrgMember
    | Failure Http.Error PotentialOrgMember
    | Success PotentialOrgMember


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
    | SetRole OrgRole
    | AddMember
    | AddMemberFinished (HttpResult ())
    | BackToFindUser


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | AddedMember OrgMember


update : AppContext -> UserHandle -> List OrgMember -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext orgHandle currentMembers msg model =
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
                        userHandles m =
                            case m of
                                OrgMember.UserMember { user } ->
                                    user.handle

                        usersToIgnore =
                            account.handle :: List.map userHandles currentMembers

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
            ( AssignRole { user = user, role = Viewer }, Cmd.none, NoOutMsg )

        SetRole role ->
            case model of
                AssignRole { user } ->
                    ( AssignRole { user = user, role = role }, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        AddMember ->
            case model of
                AssignRole member ->
                    ( Saving member, addMember appContext orgHandle member, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        AddMemberFinished res ->
            case ( model, res ) of
                ( Saving member, Ok _ ) ->
                    ( Success member, Cmd.none, AddedMember (OrgMember.UserMember { user = member.user, role = member.role }) )

                ( Saving member, Err e ) ->
                    ( Failure e member, Cmd.none, NoOutMsg )

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
                [ when tag ((==) "user") (Decode.map Just User.decodeSummaryWithId)
                , Decode.succeed Nothing
                ]
    in
    ShareApi.search query_
        |> HttpApi.toRequest (Decode.list decodeMatch) (Result.map MaybeE.values >> FetchUsersFinished query)
        |> HttpApi.perform appContext.api


addMember : AppContext -> UserHandle -> PotentialOrgMember -> Cmd Msg
addMember appContext orgHandle member =
    ShareApi.createOrgRoleMember orgHandle [ OrgMember.UserMember { user = member.user, role = member.role } ]
        |> HttpApi.toRequestWithEmptyResponse AddMemberFinished
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
                |> Modal.modal "add-org-member-modal" CloseModal
                |> Modal.withHeader "Add member"

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

                AssignRole { user, role } ->
                    let
                        options =
                            NEL.singleton Admin
                                |> NEL.cons Maintainer
                                |> NEL.cons Contributor
                                |> NEL.cons Viewer
                                |> NEL.map (\r -> RadioField.option (OrgRole.toString r) (OrgRole.description r) r)
                    in
                    modal_
                        (div [ class "assign-role" ]
                            [ div [ class "selected-user" ]
                                [ viewUser user
                                , Button.icon BackToFindUser Icon.trash
                                    |> Button.small
                                    |> Button.subdued
                                    |> Button.view
                                ]
                            , h3 [] [ text "Select a role" ]
                            , RadioField.field "Select role" SetRole options role
                                |> RadioField.view
                            ]
                        )
                        |> Modal.withActions
                            [ Button.button CloseModal "Cancel"
                                |> Button.subdued
                            , Button.button AddMember ("Add " ++ User.name user)
                                |> Button.emphasized
                            ]

                Saving member ->
                    modal_
                        (div []
                            [ text
                                ("Adding "
                                    ++ User.name member.user
                                    ++ " with "
                                    ++ OrgRole.toString member.role
                                    ++ " role"
                                )
                            ]
                        )

                Failure _ member ->
                    modal_
                        (div []
                            [ text
                                ("Failed to add "
                                    ++ User.name member.user
                                    ++ " with "
                                    ++ OrgRole.toString member.role
                                    ++ " role"
                                )
                            ]
                        )

                Success member ->
                    modal_
                        (StatusBanner.good
                            ("Successfully added "
                                ++ User.name member.user
                                ++ " with "
                                ++ OrgRole.toString member.role
                                ++ " role"
                            )
                        )
    in
    Modal.view modal
