module UnisonShare.Page.OrgPeoplePage exposing (..)

import ConfirmDelete exposing (ConfirmDeletes)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle exposing (UserHandle)
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
import UI.Tooltip as Tooltip
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
    , confirmDeletes : ConfirmDeletes
    }


init : AppContext -> UserHandle -> ( Model, Cmd Msg )
init appContext handle =
    ( { modal = NoModal
      , members = Loading
      , confirmDeletes = ConfirmDelete.emptyDeletes
      }
    , fetchMembers appContext handle
    )



-- UPDATE


type Msg
    = FetchMembersFinished (WebData (List OrgMember))
    | ShowAddMemberModal
    | CloseModal
    | RequestToRemoveMember UserHandle
    | ConfirmRemoveMember UserHandle OrgMember
    | CancelRemoveMember UserHandle
    | RemoveMemberFinished UserHandle (HttpResult ())
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

        RequestToRemoveMember handle ->
            ( { model
                | confirmDeletes =
                    ConfirmDelete.set (UserHandle.toString handle)
                        ConfirmDelete.confirm
                        model.confirmDeletes
              }
            , Cmd.none
            )

        ConfirmRemoveMember handle member ->
            ( { model
                | confirmDeletes =
                    ConfirmDelete.set
                        (UserHandle.toString handle)
                        ConfirmDelete.deleting
                        model.confirmDeletes
              }
            , removeMember appContext orgHandle handle member
            )

        RemoveMemberFinished handle res ->
            case res of
                Ok _ ->
                    let
                        withoutRemovedMember m =
                            case m of
                                OrgMember.UserMember { user } ->
                                    not (UserHandle.equals user.handle handle)

                                _ ->
                                    True

                        members =
                            RemoteData.map
                                (List.filter withoutRemovedMember)
                                model.members
                    in
                    ( { model
                        | members = members
                        , confirmDeletes =
                            ConfirmDelete.remove (UserHandle.toString handle) model.confirmDeletes
                      }
                    , Cmd.none
                    )

                Err e ->
                    ( { model
                        | confirmDeletes =
                            ConfirmDelete.set (UserHandle.toString handle)
                                (ConfirmDelete.DeleteFailed e)
                                model.confirmDeletes
                      }
                    , Cmd.none
                    )

        CancelRemoveMember handle ->
            ( { model
                | confirmDeletes =
                    ConfirmDelete.remove (UserHandle.toString handle) model.confirmDeletes
              }
            , Cmd.none
            )

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
                                    , Cmd.batch [ Cmd.map AddOrgMemberModalMsg cmd, Util.delayMsg 1500 CloseModal ]
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


removeMember : AppContext -> UserHandle -> UserHandle -> OrgMember -> Cmd Msg
removeMember appContext orgHandle memberHandle member =
    ShareApi.deleteOrgRoleAssignment orgHandle member
        |> HttpApi.toRequestWithEmptyResponse (RemoveMemberFinished memberHandle)
        |> HttpApi.perform appContext.api



-- VIEW


viewRole : OrgRole -> Html msg
viewRole role =
    Tooltip.text (OrgRole.description role)
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start
        |> Tooltip.view (role |> OrgRole.toString |> text)


viewUserMember : ConfirmDeletes -> Bool -> { user : UserSummaryWithId, roles : List OrgRole } -> Html Msg
viewUserMember deletes isLastUser ({ user, roles } as member) =
    let
        canRemove =
            not (List.member OrgRole.Owner roles) && not isLastUser

        remove =
            if canRemove then
                case ConfirmDelete.get (UserHandle.toString user.handle) deletes of
                    Just cd ->
                        ConfirmDelete.view
                            { confirmMsg = ConfirmRemoveMember user.handle (OrgMember.UserMember member)
                            , cancelMsg = CancelRemoveMember user.handle
                            }
                            cd

                    _ ->
                        Button.icon
                            (RequestToRemoveMember user.handle)
                            Icon.trash
                            |> Button.small
                            |> Button.subdued
                            |> Button.view

            else
                UI.nothing
    in
    div [ class "member" ]
        [ div [ class "member_profile-snippet" ]
            [ ProfileSnippet.profileSnippet user |> ProfileSnippet.view ]
        , div [ class "member_role" ] (roles |> List.map viewRole |> List.intersperse (text ", "))
        , div [ class "member_remove" ] [ remove ]
        ]


viewMember : ConfirmDeletes -> Bool -> OrgMember -> Html Msg
viewMember deletes isLastUser member =
    case member of
        OrgMember.UserMember m ->
            viewUserMember deletes isLastUser m

        OrgMember.TeamMember _ ->
            -- TODO: Teams are not yet fully supported
            UI.nothing


viewContent : Model -> List (Html Msg)
viewContent model =
    let
        card =
            case model.members of
                Success members ->
                    let
                        hasJustOneMember =
                            List.length members == 1
                    in
                    Card.card
                        [ div [ class "members_list" ]
                            (List.map (viewMember model.confirmDeletes hasJustOneMember) members)
                        ]
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
