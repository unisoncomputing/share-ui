module UnisonShare.Page.ProjectSettingsPage exposing (..)

import Html exposing (div, footer, h2, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import List.Nonempty as NEL
import RemoteData exposing (WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.ErrorCard as ErrorCard
import UI.Form.RadioField as RadioField
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project exposing (ProjectDetails, ProjectVisibility(..))
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
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


type alias Model =
    { form : Form }


init : Model
init =
    { form = NoChanges }



-- UPDATE


type Msg
    = UpdateVisibility ProjectVisibility
    | DiscardChanges
    | SaveChanges
    | SaveFinished (HttpResult ())
    | ClearAfterSave
    | ShowDeleteProjectModal
    | CloseModal


type OutMsg
    = None
    | ProjectUpdated ProjectDetails
    | ShowDeleteProjectModalRequest


update : AppContext -> ProjectDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext project msg model =
    case ( msg, model.form ) of
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

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


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
    in
    PageContent.oneColumn
        [ div [ class "settings-content", class stateClass ]
            [ form
            , footer [ class "actions" ]
                [ message
                , div [ class "buttons" ]
                    [ buttons.discard |> Button.view
                    , buttons.save |> Button.view
                    ]
                ]
            ]
        ]
        |> PageContent.withPageTitle
            (PageTitle.withRightSide
                [ Button.iconThenLabel ShowDeleteProjectModal Icon.trash "Delete Project"
                    |> Button.small
                    |> Button.critical
                    |> Button.view
                ]
                pageTitle
            )


view : Session -> ProjectDetails -> Model -> PageLayout Msg
view session project model =
    if Session.hasProjectAccess project.ref session then
        PageLayout.centeredNarrowLayout
            (viewPageContent project model)
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground

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
        PageLayout.centeredNarrowLayout
            (PageContent.oneColumn
                [ ErrorCard.errorCard errorTitle errorMessage
                    |> ErrorCard.toCard
                    |> Card.asContainedWithFade
                    |> Card.view
                ]
            )
            PageFooter.pageFooter
            |> PageLayout.withSubduedBackground
