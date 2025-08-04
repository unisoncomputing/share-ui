module UnisonShare.AddProjectWebhookModal exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi exposing (HttpResult)
import List.Extra as ListE
import List.Nonempty as NEL
import UI
import UI.Button as Button
import UI.Divider as Divider
import UI.Form.CheckboxField as CheckboxField
import UI.Form.RadioField as RadioField
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusBanner as StatusBanner
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectWebhook exposing (ProjectWebhook)
import UnisonShare.User exposing (UserSummaryWithId)


type NotificationEventType
    = ProjectContributionCreated
    | ProjectContributionUpdated
    | ProjectContributionComment
    | ProjectTicketCreated
    | ProjectTicketUpdated
    | ProjectTicketComment
    | ProjectBranchUpdated
    | ProjectReleaseCreated


type WebhookEvents
    = AllEvents
    | SpecificEvents (List NotificationEventType)


type alias Form =
    { url : String
    , events : WebhookEvents
    , isActive : Bool
    }


type Model
    = Edit Form
    | Saving Form
    | Failure Http.Error Form
    | Success ProjectWebhook


init : Model
init =
    Edit { url = "", events = AllEvents, isActive = True }



-- UPDATE


type Msg
    = CloseModal
    | UpdateUrl String
    | SetWebhookEvents WebhookEvents
    | ToggleEvent NotificationEventType
    | ToggleIsActive
    | AddWebhook
    | AddWebhookFinished (HttpResult ())


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | AddedWebhook ProjectWebhook


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update _ _ msg model =
    case ( msg, model ) of
        ( UpdateUrl url, Edit f ) ->
            ( Edit { f | url = url }, Cmd.none, NoOutMsg )

        ( SetWebhookEvents events, Edit f ) ->
            ( Edit { f | events = events }, Cmd.none, NoOutMsg )

        ( ToggleEvent eventType, Edit f ) ->
            let
                events =
                    case f.events of
                        AllEvents ->
                            SpecificEvents [ eventType ]

                        SpecificEvents evts ->
                            if List.member eventType evts then
                                SpecificEvents (ListE.remove eventType evts)

                            else
                                SpecificEvents (evts ++ [ eventType ])
            in
            ( Edit { f | events = events }, Cmd.none, NoOutMsg )

        ( ToggleIsActive, Edit f ) ->
            ( Edit { f | isActive = not f.isActive }, Cmd.none, NoOutMsg )

        ( AddWebhook, _ ) ->
            ( model, Cmd.none, NoOutMsg )

        ( AddWebhookFinished _, _ ) ->
            ( model, Cmd.none, NoOutMsg )

        ( CloseModal, _ ) ->
            ( model, Cmd.none, RequestCloseModal )

        _ ->
            ( model, Cmd.none, NoOutMsg )



-- EFFECTS
-- VIEW


viewUser : UserSummaryWithId -> Html msg
viewUser user =
    ProfileSnippet.profileSnippet user |> ProfileSnippet.view


divider : Html msg
divider =
    Divider.divider |> Divider.small |> Divider.view


viewEventSelection : List NotificationEventType -> Html Msg
viewEventSelection selected =
    let
        isSelected event =
            List.member event selected

        checkbox title event =
            CheckboxField.field title (ToggleEvent event) (isSelected event)
                |> CheckboxField.view

        contributionEvents =
            [ checkbox "Contribution created" ProjectContributionCreated
            , checkbox "Contribution updated" ProjectContributionUpdated
            , checkbox "Contribution comment" ProjectContributionComment
            ]

        ticketEvents =
            [ checkbox "Ticket created" ProjectTicketCreated
            , checkbox "Ticket updated" ProjectTicketUpdated
            , checkbox "Ticket comment" ProjectTicketComment
            ]

        eventSelectionCheckboxes =
            div [ class "event-selection_groups" ]
                [ div []
                    [ div [ class "checkboxes" ]
                        [ checkbox "Branch updated" ProjectBranchUpdated
                        , checkbox "Release created" ProjectReleaseCreated
                        ]
                    ]
                , div []
                    [ div [ class "checkboxes" ] contributionEvents
                    ]
                , div []
                    [ div [ class "checkboxes" ] ticketEvents
                    ]
                ]
    in
    div [ class "event-selection" ] [ divider, eventSelectionCheckboxes ]


view : Model -> Html Msg
view model =
    let
        modal_ c =
            Modal.content c
                |> Modal.modal "add-project-webhook-modal" CloseModal
                |> Modal.withHeader "Add Webhook"

        modal =
            case model of
                Edit form ->
                    let
                        specificEventsOption =
                            case form.events of
                                AllEvents ->
                                    SpecificEvents []

                                _ ->
                                    form.events

                        options =
                            NEL.singleton (RadioField.option "Select specific events" "The webhook is only called on selected events" specificEventsOption)
                                |> NEL.cons (RadioField.option "All events" "The webhook is called on all project events (including future additions)" AllEvents)

                        eventSelection =
                            case form.events of
                                AllEvents ->
                                    UI.nothing

                                SpecificEvents selected ->
                                    viewEventSelection selected
                    in
                    modal_
                        (div []
                            [ TextField.field UpdateUrl "Webhook URL" form.url
                                |> TextField.withIcon Icon.wireframeGlobe
                                |> TextField.withHelpText "This URL will be called when the selected events are triggered."
                                |> TextField.view
                            , divider
                            , RadioField.field "Events" SetWebhookEvents options form.events |> RadioField.view
                            , eventSelection
                            , divider
                            , CheckboxField.field "Active" ToggleIsActive form.isActive
                                |> CheckboxField.withHelpText "Actively call the Webhook URL when selected events are triggered."
                                |> CheckboxField.view
                            ]
                        )
                        |> Modal.withActions
                            [ Button.button CloseModal "Cancel"
                                |> Button.subdued
                            , Button.button AddWebhook "Add Webhook"
                                |> Button.emphasized
                            ]
                        |> Modal.withLeftSideFooter
                            [ Button.iconThenLabel_ Link.docs Icon.docs "Webhook request format docs"
                                |> Button.small
                                |> Button.outlined
                                |> Button.view
                            ]

                Saving _ ->
                    modal_ (StatusBanner.working "Adding Webhook...")

                Failure _ _ ->
                    modal_ (StatusBanner.bad "Failed to add Webhook")

                Success _ ->
                    modal_ (StatusBanner.good "Successfully added Webhook")
    in
    Modal.view modal
