module UnisonShare.Page.OrgPeoplePage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle exposing (UserHandle)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.AddOrgMemberModal as AddOrgMemberModal
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.OrgMember as OrgMember exposing (OrgMember)
import UnisonShare.OrgRole as OrgRole exposing (OrgRole)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.User exposing (UserSummaryWithId)



-- MODEL


type Modal
    = NoModal
    | AddMemberModal AddOrgMemberModal.Model


type alias Model =
    { modal : Modal
    , members : WebData (List OrgMember)
    }


init : AppContext -> UserHandle -> ( Model, Cmd Msg )
init appContext handle =
    ( { modal = NoModal, members = Loading }
    , fetchMembers appContext handle
    )



-- UPDATE


type Msg
    = FetchMembersFinished (WebData (List OrgMember))
    | ShowAddMemberModal
    | CloseModal
    | RemoveMember OrgMember
    | RemoveMemberFinished (HttpResult ())
    | AddOrgMemberModalMsg AddOrgMemberModal.Msg


update : AppContext -> UserHandle -> Msg -> Model -> ( Model, Cmd Msg )
update appContext orgHandle msg model =
    case msg of
        FetchMembersFinished members ->
            ( { model | members = members }, Cmd.none )

        ShowAddMemberModal ->
            ( { model | modal = AddMemberModal AddOrgMemberModal.init }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        RemoveMember member ->
            ( model, removeMember appContext orgHandle member )

        RemoveMemberFinished _ ->
            ( model, Cmd.none )

        AddOrgMemberModalMsg memberModalMsg ->
            case model.modal of
                AddMemberModal m ->
                    case model.members of
                        Success members ->
                            let
                                ( memberModal, cmd, out ) =
                                    AddOrgMemberModal.update appContext orgHandle members memberModalMsg m
                            in
                            case out of
                                AddOrgMemberModal.NoOutMsg ->
                                    ( { model | modal = AddMemberModal memberModal }
                                    , Cmd.map AddOrgMemberModalMsg cmd
                                    )

                                AddOrgMemberModal.RequestCloseModal ->
                                    ( { model | modal = NoModal }, Cmd.none )

                                AddOrgMemberModal.AddedMember newMember ->
                                    ( { model | modal = AddMemberModal memberModal, members = Success (newMember :: members) }
                                    , Cmd.map AddOrgMemberModalMsg cmd
                                    )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- EFFECTS


fetchMembers : AppContext -> UserHandle -> Cmd Msg
fetchMembers appContext orgHandle =
    ShareApi.orgRoleAssignments orgHandle
        |> HttpApi.toRequest
            (Decode.field "role_assignments" OrgMember.decodeList)
            (RemoteData.fromResult >> FetchMembersFinished)
        |> HttpApi.perform appContext.api


removeMember : AppContext -> UserHandle -> OrgMember -> Cmd Msg
removeMember appContext orgHandle member =
    ShareApi.deleteOrgRoleAssignment orgHandle member
        |> HttpApi.toRequestWithEmptyResponse RemoveMemberFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewUserMember : { user : UserSummaryWithId, roles : List OrgRole } -> Html Msg
viewUserMember ({ user, roles } as member) =
    div [ class "member" ]
        [ div [ class "member_profile-snippet" ]
            [ ProfileSnippet.profileSnippet user |> ProfileSnippet.view ]
        , div [ class "member_role" ] [ text (roles |> List.map OrgRole.toString |> String.join ", ") ]
        , Button.icon (RemoveMember (OrgMember.UserMember member)) Icon.trash
            |> Button.small
            |> Button.subdued
            |> Button.view
        ]


viewMember : OrgMember -> Html Msg
viewMember member =
    case member of
        OrgMember.UserMember m ->
            viewUserMember m

        OrgMember.TeamMember _ ->
            -- TODO
            UI.nothing


viewContent : Model -> List (Html Msg)
viewContent model =
    let
        card =
            case model.members of
                Success members ->
                    Card.card [ div [ class "members_list" ] (List.map viewMember members) ]
                        |> Card.withClassName "members"

                Failure e ->
                    Card.card [ text (Util.httpErrorToString e) ]

                _ ->
                    Card.card [ text "TODO LOADING" ]
    in
    [ card
        |> Card.asContained
        |> Card.view
    ]


viewPageTitle : PageTitle Msg
viewPageTitle =
    PageTitle.title "People"
        |> PageTitle.withRightSide
            [ Button.iconThenLabel
                ShowAddMemberModal
                Icon.plus
                "Add an organization member"
                |> Button.view
            ]


view : Model -> ( PageLayout Msg, Maybe (Html Msg) )
view model =
    let
        content =
            PageContent.oneColumn (viewContent model)
                |> PageContent.withPageTitle viewPageTitle

        layout =
            PageLayout.centeredNarrowLayout content
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground

        modal =
            case model.modal of
                AddMemberModal m ->
                    Just (Html.map AddOrgMemberModalMsg (AddOrgMemberModal.view m))

                NoModal ->
                    Nothing
    in
    ( layout, modal )
