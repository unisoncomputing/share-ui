module UnisonShare.Page.ProjectSettingsPage exposing (..)

import Html exposing (Html, div, footer, h2, header, strong, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode
import Lib.Decode.Helpers exposing (whenKindIs)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle exposing (UserHandle)
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
import UI.Tag as Tag
import UnisonShare.AddProjectCollaboratorModal as AddProjectCollaboratorModal
import UnisonShare.AddProjectWebhookModal as AddProjectWebhookModal
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.ErrorDetails as ErrorDetails
import UnisonShare.NotificationTopicType as NotificationTopicType
import UnisonShare.Org as Org exposing (OrgSummary)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectDetails, ProjectVisibility(..))
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectCollaborator as ProjectCollaborator exposing (ProjectCollaborator)
import UnisonShare.ProjectRole as ProjectRole
import UnisonShare.ProjectWebhook as ProjectWebhook exposing (ProjectWebhook)
import UnisonShare.ProjectWebhookExamplesModal as ProjectWebhookExamplesModal
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.User as User exposing (UserSummary)
import Url



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
    | AddWebhookModal AddProjectWebhookModal.Model
    | WebhookExamplesModal ProjectWebhookExamplesModal.Model


type ProjectOwner
    = UserOwner UserSummary
    | OrgOwner OrgSummary


type alias Model =
    { collaborators : WebData (List ProjectCollaborator)
    , webhooks : WebData (List ProjectWebhook)
    , owner : WebData ProjectOwner
    , modal : Modal
    , form : Form
    }


{-| Unlike `init` this doesn't fetch collaboratos, as doing so eventually
requires the project (because of `update`) from the parent ProjectPage to be
fetched and would cause a race condition. `init` itself is only used when
switching between project subpages when the project data is already fetched.
-}
preInit : Model
preInit =
    { collaborators = NotAsked
    , webhooks = Success []
    , owner = NotAsked
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
      , webhooks = Success []
      , owner = Loading
      , modal = NoModal
      , form = NoChanges
      }
    , Cmd.batch
        [ fetchOwner appContext (ProjectRef.handle projectRef)
        , fetchCollaborators appContext projectRef
        , fetchWebhooks appContext projectRef
        ]
    )



-- UPDATE


type Msg
    = FetchCollaboratorsFinished (WebData (List ProjectCollaborator))
    | FetchOwnerFinished (WebData ProjectOwner)
    | FetchProjectWebhooksFinished (WebData (List ProjectWebhook))
    | UpdateVisibility ProjectVisibility
    | DiscardChanges
    | SaveChanges
    | SaveFinished (HttpResult ())
    | ClearAfterSave
    | ShowDeleteProjectModal
    | ShowAddCollaboratorModal
    | ShowAddWebhookModal
    | ShowWebhookExamplesModal
    | CloseModal
    | RemoveCollaborator ProjectCollaborator
    | RemoveWebhook ProjectWebhook
    | RemoveCollaboratorFinished (HttpResult ())
    | RemoveWebhookFinished (HttpResult ())
    | AddProjectCollaboratorModalMsg AddProjectCollaboratorModal.Msg
    | AddProjectWebhookModalMsg AddProjectWebhookModal.Msg
    | ProjectWebhookExamplesModalMsg ProjectWebhookExamplesModal.Msg


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
            ( { model | collaborators = withoutOwner }, Cmd.none, None )

        ( FetchOwnerFinished owner, _ ) ->
            ( { model | owner = owner }, Cmd.none, None )

        ( FetchProjectWebhooksFinished webhooks, _ ) ->
            ( { model | webhooks = webhooks }, Cmd.none, None )

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

        ( ShowAddWebhookModal, _ ) ->
            ( { model | modal = AddWebhookModal AddProjectWebhookModal.init }, Cmd.none, None )

        ( ShowWebhookExamplesModal, _ ) ->
            let
                ( examples, cmd ) =
                    ProjectWebhookExamplesModal.init appContext project.ref
            in
            ( { model | modal = WebhookExamplesModal examples }, Cmd.map ProjectWebhookExamplesModalMsg cmd, None )

        ( CloseModal, _ ) ->
            ( { model | modal = NoModal }, Cmd.none, None )

        ( RemoveCollaborator collab, _ ) ->
            let
                collaborators =
                    model.collaborators
                        |> RemoteData.map (List.filter (\c -> c /= collab))
            in
            ( { model | collaborators = collaborators }, removeCollaborator appContext project.ref collab, None )

        ( RemoveWebhook webhook, _ ) ->
            let
                webhooks =
                    model.webhooks
                        |> RemoteData.map (List.filter (\w -> w /= webhook))
            in
            ( { model | webhooks = webhooks }, removeWebhook appContext project.ref webhook, None )

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

        ( AddProjectWebhookModalMsg webhookMsg, _ ) ->
            case ( model.modal, model.webhooks ) of
                ( AddWebhookModal m, Success currentWebhooks ) ->
                    let
                        ( modal, cmd, out ) =
                            AddProjectWebhookModal.update
                                appContext
                                project.ref
                                webhookMsg
                                m
                    in
                    case out of
                        AddProjectWebhookModal.NoOutMsg ->
                            ( { model | modal = AddWebhookModal modal }
                            , Cmd.map AddProjectWebhookModalMsg cmd
                            , None
                            )

                        AddProjectWebhookModal.RequestCloseModal ->
                            ( { model | modal = NoModal }
                            , Cmd.map AddProjectWebhookModalMsg cmd
                            , None
                            )

                        AddProjectWebhookModal.AddedWebhook webhook ->
                            let
                                webhooks =
                                    Success (webhook :: currentWebhooks)
                            in
                            ( { model | modal = AddWebhookModal modal, webhooks = webhooks }
                            , Cmd.batch [ Cmd.map AddProjectWebhookModalMsg cmd, Util.delayMsg 1000 CloseModal ]
                            , None
                            )

                _ ->
                    ( model, Cmd.none, None )

        ( ProjectWebhookExamplesModalMsg examplesMsg, _ ) ->
            case model.modal of
                WebhookExamplesModal m ->
                    let
                        ( modal, cmd, out ) =
                            ProjectWebhookExamplesModal.update appContext project.ref examplesMsg m
                    in
                    case out of
                        ProjectWebhookExamplesModal.NoOutMsg ->
                            ( { model | modal = WebhookExamplesModal modal }
                            , Cmd.map ProjectWebhookExamplesModalMsg cmd
                            , None
                            )

                        ProjectWebhookExamplesModal.RequestCloseModal ->
                            ( { model | modal = NoModal }
                            , Cmd.map ProjectWebhookExamplesModalMsg cmd
                            , None
                            )

                _ ->
                    ( model, Cmd.none, None )

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


{-| Used by parent, ProjectPage
-}
fetchProjectCollaborators : AppContext -> ProjectRef -> Model -> ( Model, Cmd Msg )
fetchProjectCollaborators appContext projectRef model =
    ( { model | collaborators = Loading }, fetchCollaborators appContext projectRef )


{-| Used by parent, ProjectPage
-}
fetchProjectOwner : AppContext -> ProjectRef -> Model -> ( Model, Cmd Msg )
fetchProjectOwner appContext projectRef model =
    ( { model | owner = Loading }, fetchOwner appContext (ProjectRef.handle projectRef) )


{-| Used by parent, ProjectPage
-}
fetchProjectWebhooks : AppContext -> ProjectRef -> Model -> ( Model, Cmd Msg )
fetchProjectWebhooks appContext projectRef model =
    ( { model | webhooks = Loading }, fetchWebhooks appContext projectRef )


fetchCollaborators : AppContext -> ProjectRef -> Cmd Msg
fetchCollaborators appContext projectRef =
    ShareApi.projectRoleAssignments projectRef
        |> HttpApi.toRequest
            (Decode.field "role_assignments" (Decode.list ProjectCollaborator.decode))
            (RemoteData.fromResult >> FetchCollaboratorsFinished)
        |> HttpApi.perform appContext.api


fetchOwner : AppContext -> UserHandle -> Cmd Msg
fetchOwner appContext handle =
    let
        decode =
            Decode.oneOf
                [ whenKindIs "org" (Decode.map OrgOwner Org.decodeSummary)
                , whenKindIs "user" (Decode.map UserOwner User.decodeSummary)
                ]
    in
    ShareApi.user handle
        |> HttpApi.toRequest decode (RemoteData.fromResult >> FetchOwnerFinished)
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


fetchWebhooks : AppContext -> ProjectRef -> Cmd Msg
fetchWebhooks appContext projectRef =
    ShareApi.projectWebhooks projectRef
        |> HttpApi.toRequest
            (Decode.field "webhooks" (Decode.list ProjectWebhook.decode))
            (RemoteData.fromResult >> FetchProjectWebhooksFinished)
        |> HttpApi.perform appContext.api


removeWebhook : AppContext -> ProjectRef -> ProjectWebhook -> Cmd Msg
removeWebhook appContext projectRef webhook =
    ShareApi.deleteProjectWebhook projectRef webhook
        |> HttpApi.toRequestWithEmptyResponse RemoveWebhookFinished
        |> HttpApi.perform appContext.api



-- VIEW


pageTitle : PageTitle.PageTitle msg
pageTitle =
    PageTitle.title "Project Settings"
        |> PageTitle.withDescription "Manage your project. Collaborators, webhooks, and visibility."


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


viewCollaborators : Session -> Model -> Html Msg
viewCollaborators session model =
    let
        ( collabs, addButton ) =
            case model.collaborators of
                Success collaborators ->
                    let
                        addButton_ =
                            Button.iconThenLabel ShowAddCollaboratorModal Icon.plus "Add a collaborator"
                                |> Button.small
                                |> Button.view

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
                                ( div [ class "list_empty-state" ]
                                    [ div [ class "list_empty-state_text" ]
                                        [ Icon.view Icon.userGroup, text "You haven't invited any collaborators yet" ]
                                    ]
                                , addButton_
                                )

                            else
                                ( div [ class "collaborators" ]
                                    [ Divider.divider
                                        |> Divider.withoutMargin
                                        |> Divider.small
                                        |> Divider.view
                                    , div [ class "collaborators_list" ]
                                        (List.map viewCollaborator collaborators)
                                    ]
                                , addButton_
                                )
                    in
                    content

                Failure e ->
                    ( div [ class "collaborators_error" ]
                        [ StatusBanner.bad "Could not load collaborators"
                        , ErrorDetails.view session e
                        ]
                    , UI.nothing
                    )

                _ ->
                    ( div [ class "list_loading" ]
                        [ Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
                        ]
                    , UI.nothing
                    )
    in
    Card.card
        [ header [ class "project-settings_card_header" ] [ h2 [] [ text "Collaborators" ], addButton ]
        , collabs
        ]
        |> Card.asContained
        |> Card.view


viewWebhook : ProjectWebhook -> Html Msg
viewWebhook webhook =
    let
        topicIcon event =
            if String.startsWith "Branch" event then
                Icon.branch

            else if String.startsWith "Contribution" event then
                Icon.merge

            else if String.startsWith "Ticket" event then
                Icon.bug

            else
                Icon.bolt

        viewTopicTag topic =
            Tag.tag topic |> Tag.withIcon (topicIcon topic) |> Tag.view

        viewAction msg icon =
            Button.icon msg icon
                |> Button.small
                |> Button.subdued
                |> Button.view

        viewTopics =
            case webhook.topics of
                ProjectWebhook.AllTopics ->
                    [ viewTopicTag "All" ]

                ProjectWebhook.SelectedTopics topics ->
                    List.map
                        (NotificationTopicType.toString >> viewTopicTag)
                        topics
    in
    div [ class "webhook" ]
        [ div
            [ class "webhook_details" ]
            [ strong [ class "webhook_url" ] [ Icon.view Icon.chain, text (Url.toString webhook.url) ]
            , div [ class "webhook_events" ] viewTopics
            ]
        , div
            [ class "webhook_actions" ]
            [ viewAction (RemoveWebhook webhook) Icon.trash
            ]
        ]


viewWebhooks : Session -> Model -> Html Msg
viewWebhooks session model =
    let
        divider =
            Divider.divider |> Divider.small |> Divider.view

        {-
           examplesButton =
               Button.iconThenLabel ShowWebhookExamplesModal Icon.docs "Webhook example docs"
                   |> Button.small
                   |> Button.subdued
                   |> Button.view
        -}
        addButton =
            Button.iconThenLabel ShowAddWebhookModal Icon.plus "Add a webhook"
                |> Button.small
                |> Button.view

        content =
            case model.webhooks of
                Success webhooks ->
                    if List.isEmpty webhooks then
                        [ header [ class "project-settings_card_header" ] [ h2 [] [ text "Webhooks" ], div [ class "webhook-buttons" ] [ addButton ] ]
                        , div [ class "list_empty-state" ]
                            [ div [ class "list_empty-state_text" ]
                                [ Icon.view Icon.wireframeGlobe, text "You haven't set up any webhooks yet" ]
                            ]
                        ]

                    else
                        [ header [ class "project-settings_card_header" ] [ h2 [] [ text "Webhooks" ], div [ class "webhook-buttons" ] [ addButton ] ]
                        , div [ class "webhooks" ] (webhooks |> List.map viewWebhook |> List.intersperse divider)
                        ]

                Failure e ->
                    [ StatusBanner.bad "Could not load webhooks."
                    , ErrorDetails.view session e
                    ]

                _ ->
                    [ div [ class "list_loading" ]
                        [ Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
                        , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
                        ]
                    ]
    in
    Card.card
        content
        |> Card.asContained
        |> Card.view


viewPageContent : Session -> ProjectDetails -> Model -> PageContent Msg
viewPageContent session project model =
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

        privateDescription =
            case model.owner of
                Success (OrgOwner org) ->
                    "Only members of " ++ Org.name org ++ " can see and download this project."

                _ ->
                    "Only you can see and download this project."

        projectVisibilityField =
            RadioField.field "project-visibility"
                UpdateVisibility
                (NEL.Nonempty
                    (RadioField.option
                        "Private"
                        privateDescription
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
            let
                overlay_ =
                    div [ class "disabled-overlay" ] []

                ( overlay, message_ ) =
                    case model.owner of
                        NotAsked ->
                            ( overlay_, UI.nothing )

                        Loading ->
                            ( overlay_, UI.nothing )

                        Success (UserOwner _) ->
                            ( UI.nothing, UI.nothing )

                        Success (OrgOwner org) ->
                            if org.isCommercial then
                                ( UI.nothing, UI.nothing )

                            else
                                ( overlay_
                                , StatusBanner.info "Changing visibility is not supported for public organizations."
                                )

                        Failure e ->
                            ( overlay_
                            , div []
                                [ StatusBanner.bad "An unexpected error occurred, please try again."
                                , ErrorDetails.view session e
                                ]
                            )
            in
            Card.card
                [ h2 [] [ text "Project Visibility" ]
                , message_
                , div [ class "form" ] [ overlay, RadioField.view projectVisibilityField ]
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

        formAndActions =
            [ form
            , footer [ class "actions" ]
                [ message
                , div [ class "buttons" ]
                    [ buttons.discard |> Button.view
                    , buttons.save |> Button.view
                    ]
                ]
            ]

        collaborators =
            if Project.isPublic project || project.isPremiumProject then
                viewCollaborators session model

            else
                UI.nothing

        webhooks =
            viewWebhooks session model
    in
    PageContent.oneColumn
        [ div [ class "settings-content", class stateClass ]
            (collaborators :: webhooks :: formAndActions)
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

                    AddWebhookModal m ->
                        Just (Html.map AddProjectWebhookModalMsg (AddProjectWebhookModal.view m))

                    WebhookExamplesModal m ->
                        Just (Html.map ProjectWebhookExamplesModalMsg (ProjectWebhookExamplesModal.view session m))

                    _ ->
                        Nothing
        in
        ( PageLayout.centeredNarrowLayout
            (viewPageContent session project model)
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
