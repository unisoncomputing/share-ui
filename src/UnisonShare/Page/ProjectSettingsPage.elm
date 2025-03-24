module UnisonShare.Page.ProjectSettingsPage exposing (..)

import Html exposing (Html, div, footer, h2, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Divider as Divider
import UI.ErrorCard as ErrorCard
import UI.Form.RadioField as RadioField
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusBanner as StatusBanner
import UnisonShare.AddProjectCollaboratorModal as AddProjectCollaboratorModal
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectDetails, ProjectVisibility(..))
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectCollaborator as ProjectCollaborator exposing (ProjectCollaborator)
import UnisonShare.ProjectRole as ProjectRole
import UnisonShare.Session as Session exposing (Session)



-- MODEL


type alias Changes =
    { visibility : ProjectVisibility }


type Form
    = NoChanges
    | WithChanges Changes
    | Saving Changes
    | SaveSuccessful
    | SaveFailed Changes Error


type alias DeleteProject =
    { confirmText : String, deleting : WebData () }


type Modal
    = NoModal
    | AddCollaboratorModal AddProjectCollaboratorModal.Model


type alias Model =
    { collaborators : WebData (List ProjectCollaborator)
    , modal : Modal
    , form : Form
    }


{-| Unlike `init` this doesn't fetch collaboratos, as doing so eventually
requires the project from the parent ProjectPage to be fetched and would cause
a race condition. `init` it self is only used when switching between project
subpages when the project data is already fetched.
-}
preInit : Model
preInit =
    { collaborators = NotAsked
    , modal = NoModal
    , form = NoChanges
    }


{-| `init` with the fetching of collaborators depends on the parent page
(ProjectPage) having the project data fetched already (otherwise
`ProjectSettingsPage.update` is not run). Therefore, only use `init` when that
project data is already fetched. For instance when switching between sub
project pages.
-}
init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    ( { collaborators = Loading
      , modal = NoModal
      , form = NoChanges
      }
    , fetchCollaborators appContext projectRef
    )



-- UPDATE


type Msg
    = FetchCollaboratorsFinished (WebData (List ProjectCollaborator))
    | UpdateVisibility ProjectVisibility
    | DiscardChanges
    | SaveChanges
    | SaveFinished (HttpResult ())
    | ClearAfterSave
    | ShowDeleteProjectModal
    | ShowAddCollaboratorModal
    | CloseModal
    | RemoveCollaborator ProjectCollaborator
    | RemoveCollaboratorFinished (HttpResult ())
    | AddProjectCollaboratorModalMsg AddProjectCollaboratorModal.Msg


type OutMsg
    = None
    | ProjectUpdated ProjectDetails
    | ShowDeleteProjectModalRequest


update : AppContext -> ProjectDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext project msg model =
    case ( msg, model.form ) of
        ( FetchCollaboratorsFinished collabs, _ ) ->
            let
                withoutOwner =
                    collabs
                        |> RemoteData.map
                            (List.filter (\collab -> not (List.member ProjectRole.Owner collab.roles)))
            in
            ( { model | collaborators = withoutOwner, form = NoChanges }, Cmd.none, None )

        ( UpdateVisibility newVisibility, WithChanges c ) ->
            if newVisibility /= project.visibility then
                ( { model | form = WithChanges { c | visibility = newVisibility } }, Cmd.none, None )

            else
                ( { model | form = NoChanges }, Cmd.none, None )

        ( UpdateVisibility newVisibility, _ ) ->
            ( { model | form = WithChanges { visibility = newVisibility } }, Cmd.none, None )

        ( DiscardChanges, WithChanges _ ) ->
            ( { model | form = NoChanges }, Cmd.none, None )

        ( SaveChanges, WithChanges c ) ->
            ( { model | form = Saving c }, updateProjectSettings appContext project.ref c, None )

        ( SaveFinished (Ok _), Saving c ) ->
            let
                p =
                    { project | visibility = c.visibility }
            in
            ( { model | form = SaveSuccessful }, Util.delayMsg 3000 ClearAfterSave, ProjectUpdated p )

        ( SaveFinished (Err e), Saving c ) ->
            ( { model | form = SaveFailed c e }, Cmd.none, None )

        ( ClearAfterSave, SaveSuccessful ) ->
            ( { model | form = NoChanges }, Cmd.none, None )

        ( ShowDeleteProjectModal, _ ) ->
            ( model, Cmd.none, ShowDeleteProjectModalRequest )

        ( ShowAddCollaboratorModal, _ ) ->
            ( { model | modal = AddCollaboratorModal AddProjectCollaboratorModal.init }, Cmd.none, None )

        ( CloseModal, _ ) ->
            ( { model | modal = NoModal }, Cmd.none, None )

        ( RemoveCollaborator collab, _ ) ->
            let
                collaborators =
                    model.collaborators
                        |> RemoteData.map (List.filter (\c -> not (c == collab)))
            in
            ( { model | collaborators = collaborators }, removeCollaborator appContext project.ref collab, None )

        ( AddProjectCollaboratorModalMsg collabMsg, _ ) ->
            case ( model.modal, model.collaborators ) of
                ( AddCollaboratorModal m, Success currentCollabs ) ->
                    let
                        ( modal, cmd, out ) =
                            AddProjectCollaboratorModal.update appContext project.ref currentCollabs collabMsg m
                    in
                    case out of
                        AddProjectCollaboratorModal.NoOutMsg ->
                            ( { model | modal = AddCollaboratorModal modal }
                            , Cmd.map AddProjectCollaboratorModalMsg cmd
                            , None
                            )

                        AddProjectCollaboratorModal.RequestCloseModal ->
                            ( { model | modal = NoModal }
                            , Cmd.map AddProjectCollaboratorModalMsg cmd
                            , None
                            )

                        AddProjectCollaboratorModal.AddedCollaborator collab ->
                            let
                                collaborators =
                                    Success (collab :: currentCollabs)
                            in
                            ( { model | modal = AddCollaboratorModal modal, collaborators = collaborators }
                            , Cmd.batch [ Cmd.map AddProjectCollaboratorModalMsg cmd, Util.delayMsg 1500 CloseModal ]
                            , None
                            )

                _ ->
                    ( model, Cmd.none, None )

        _ ->
            ( model, Cmd.none, None )


fetchProjectCollaborators : AppContext -> ProjectRef -> Model -> ( Model, Cmd Msg )
fetchProjectCollaborators appContext projectRef model =
    ( { model | collaborators = Loading }, fetchCollaborators appContext projectRef )



-- EFFECTS


fetchCollaborators : AppContext -> ProjectRef -> Cmd Msg
fetchCollaborators appContext projectRef =
    ShareApi.projectRoleAssignments projectRef
        |> HttpApi.toRequest
            (Decode.field "role_assignments" (Decode.list ProjectCollaborator.decode))
            (RemoteData.fromResult >> FetchCollaboratorsFinished)
        |> HttpApi.perform appContext.api


removeCollaborator : AppContext -> ProjectRef -> ProjectCollaborator -> Cmd Msg
removeCollaborator appContext projectRef collab =
    ShareApi.deleteProjectRoleAssignment projectRef collab
        |> HttpApi.toRequestWithEmptyResponse RemoveCollaboratorFinished
        |> HttpApi.perform appContext.api


updateProjectSettings : AppContext -> ProjectRef -> Changes -> Cmd Msg
updateProjectSettings appContext projectRef changes =
    ShareApi.updateProject projectRef (ShareApi.ProjectSettingsUpdate changes)
        |> HttpApi.toRequestWithEmptyResponse SaveFinished
        |> HttpApi.perform appContext.api



-- VIEW


pageTitle : PageTitle.PageTitle msg
pageTitle =
    PageTitle.title "Project Settings"
        |> PageTitle.withDescription "Manage your project visibility and settings."


viewLoadingPage : PageLayout msg
viewLoadingPage =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        content =
            PageContent.oneColumn
                [ Card.card
                    [ shape Placeholder.Large
                    , shape Placeholder.Small
                    , shape Placeholder.Medium
                    ]
                    |> Card.asContained
                    |> Card.view
                ]
                |> PageContent.withPageTitle pageTitle
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewCollaborators : Model -> Html Msg
viewCollaborators model =
    let
        collabs =
            case model.collaborators of
                Success collaborators ->
                    let
                        addButton =
                            Button.iconThenLabel ShowAddCollaboratorModal Icon.plus "Add a collaborator"

                        viewCollaborator collab =
                            div [ class "collaborator" ]
                                [ div [ class "collaborator_profile-snippet" ]
                                    [ ProfileSnippet.profileSnippet collab.user
                                        |> ProfileSnippet.view
                                    ]
                                , div [ class "collaborator_role" ] [ text (collab.roles |> List.map ProjectRole.toString |> String.join ", ") ]
                                , Button.icon (RemoveCollaborator collab) Icon.trash
                                    |> Button.small
                                    |> Button.subdued
                                    |> Button.view
                                ]

                        content =
                            if List.isEmpty collaborators then
                                div [ class "collaborators_empty-state" ]
                                    [ div [ class "collaborators_empty-state_text" ]
                                        [ Icon.view Icon.userGroup, text "You haven't invited any collaborators yet" ]
                                    , Button.view addButton
                                    ]

                            else
                                div [ class "collaborators" ]
                                    [ addButton |> Button.small |> Button.view
                                    , Divider.divider
                                        |> Divider.withoutMargin
                                        |> Divider.small
                                        |> Divider.view
                                    , div [ class "collaborators_list" ]
                                        (List.map viewCollaborator collaborators)
                                    ]
                    in
                    content

                Failure _ ->
                    div [ class "collaborators_error" ]
                        [ StatusBanner.bad "Could not load collaborators"
                        ]

                _ ->
                    div [ class "collaborators_loading" ]
                        [ Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
                        ]
    in
    Card.card
        [ h2 [] [ text "Project Collaborators" ]
        , collabs
        ]
        |> Card.asContained
        |> Card.view


viewPageContent : ProjectDetails -> Model -> PageContent Msg
viewPageContent project model =
    let
        activeVisiblityValue =
            case model.form of
                NoChanges ->
                    project.visibility

                WithChanges { visibility } ->
                    visibility

                Saving { visibility } ->
                    visibility

                SaveSuccessful ->
                    project.visibility

                SaveFailed { visibility } _ ->
                    visibility

        projectVisibilityField =
            RadioField.field "project-visibility"
                UpdateVisibility
                (NEL.Nonempty
                    (RadioField.option
                        "Private"
                        "Only you can see and download this project."
                        Private
                    )
                    [ RadioField.option
                        "Public"
                        "Everyone on the internet can see and download this project."
                        Public
                    ]
                )
                activeVisiblityValue

        form =
            Card.card
                [ div [ class "form" ]
                    [ h2 []
                        [ text "Project Visibility" ]
                    , RadioField.view projectVisibilityField
                    ]
                ]
                |> Card.asContained
                |> Card.view

        buttons_ =
            { discard =
                Button.button DiscardChanges "Discard Changes"
                    |> Button.medium
                    |> Button.subdued
            , save =
                Button.button SaveChanges "Save"
                    |> Button.medium
                    |> Button.emphasized
            }

        disabledButtons =
            { discard = buttons_.discard |> Button.disabled
            , save = buttons_.save |> Button.disabled
            }

        ( buttons, stateClass, message ) =
            case model.form of
                NoChanges ->
                    ( disabledButtons, "no-changes", UI.nothing )

                WithChanges _ ->
                    ( buttons_, "with-changes", UI.nothing )

                Saving _ ->
                    ( disabledButtons, "saving", StatusBanner.working "Saving project" )

                SaveSuccessful ->
                    ( buttons_, "save-success", StatusBanner.good "Successfully saved project!" )

                SaveFailed _ _ ->
                    ( buttons_, "save-failed", StatusBanner.bad "Couldn't save project, please try again" )

        pageTitle_ =
            if Project.canDelete project then
                PageContent.withPageTitle
                    (PageTitle.withRightSide
                        [ Button.iconThenLabel ShowDeleteProjectModal Icon.trash "Delete Project"
                            |> Button.small
                            |> Button.critical
                            |> Button.view
                        ]
                        pageTitle
                    )

            else
                PageContent.withPageTitle pageTitle
    in
    PageContent.oneColumn
        [ div [ class "settings-content", class stateClass ]
            [ viewCollaborators model
            , form
            , footer [ class "actions" ]
                [ message
                , div [ class "buttons" ]
                    [ buttons.discard |> Button.view
                    , buttons.save |> Button.view
                    ]
                ]
            ]
        ]
        |> pageTitle_


view : Session -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view session project model =
    if Project.canManage project then
        let
            modal =
                case model.modal of
                    AddCollaboratorModal m ->
                        Just (Html.map AddProjectCollaboratorModalMsg (AddProjectCollaboratorModal.view m))

                    _ ->
                        Nothing
        in
        ( PageLayout.centeredNarrowLayout
            (viewPageContent project model)
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
        , modal
        )

    else
        let
            ( errorTitle, errorMessage ) =
                case session of
                    Session.Anonymous ->
                        ( "Not signed in"
                        , "You must be signed in to view Project Settings."
                        )

                    Session.SignedIn _ ->
                        ( "No access"
                        , "Sorry, but your account does not have access to Project Settings for '" ++ ProjectRef.toString project.ref ++ "'."
                        )
        in
        ( PageLayout.centeredNarrowLayout
            (PageContent.oneColumn
                [ ErrorCard.errorCard errorTitle errorMessage
                    |> ErrorCard.toCard
                    |> Card.asContainedWithFade
                    |> Card.view
                ]
            )
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
        , Nothing
        )
