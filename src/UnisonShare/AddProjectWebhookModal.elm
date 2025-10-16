module UnisonShare.AddProjectWebhookModal exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
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
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.NotificationTopicType exposing (NotificationTopicType(..))
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectWebhook as ProjectWebhook exposing (ProjectWebhook, ProjectWebhookForm, ProjectWebhookTopics(..))
import UnisonShare.User exposing (UserSummaryWithId)
import Url


type alias Form =
    { url : String
    , topics : ProjectWebhookTopics
    }


type Validation
    = NotChecked
    | Valid
    | InvalidUrlAndSelection
    | InvalidUrl
    | InvalidSelection


type Model
    = Edit Validation Form
    | Saving Form
    | Failure Http.Error Form
    | Success ProjectWebhook


init : Model
init =
    Edit NotChecked { url = "", topics = AllTopics }



-- UPDATE


type Msg
    = CloseModal
    | UpdateUrl String
    | SetProjectWebhookTopics ProjectWebhookTopics
    | ToggleTopic NotificationTopicType
    | AddWebhook
    | AddWebhookFinished (HttpResult ProjectWebhook)


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | AddedWebhook ProjectWebhook


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case ( msg, model ) of
        ( UpdateUrl url, Edit v f ) ->
            ( Edit v { f | url = url }, Cmd.none, NoOutMsg )

        ( SetProjectWebhookTopics topics, Edit v f ) ->
            ( Edit v { f | topics = topics }, Cmd.none, NoOutMsg )

        ( ToggleTopic topicType, Edit v f ) ->
            let
                topics =
                    case f.topics of
                        AllTopics ->
                            SelectedTopics [ topicType ]

                        SelectedTopics evts ->
                            if List.member topicType evts then
                                SelectedTopics (ListE.remove topicType evts)

                            else
                                SelectedTopics (evts ++ [ topicType ])
            in
            ( Edit v { f | topics = topics }, Cmd.none, NoOutMsg )

        ( AddWebhook, Edit _ f ) ->
            let
                hasTopics =
                    case f.topics of
                        AllTopics ->
                            True

                        SelectedTopics topics ->
                            not (List.isEmpty topics)
            in
            case ( Url.fromString f.url, hasTopics ) of
                ( Just url, True ) ->
                    ( Saving f, addWebhook appContext projectRef { url = url, topics = f.topics }, NoOutMsg )

                ( Just _, False ) ->
                    ( Edit InvalidSelection f, Cmd.none, NoOutMsg )

                ( Nothing, True ) ->
                    ( Edit InvalidUrl f, Cmd.none, NoOutMsg )

                ( Nothing, False ) ->
                    ( Edit InvalidUrlAndSelection f, Cmd.none, NoOutMsg )

        ( AddWebhookFinished res, Saving f ) ->
            case res of
                Ok webhook ->
                    ( Success webhook, Cmd.none, AddedWebhook webhook )

                Err e ->
                    ( Failure e f, Cmd.none, NoOutMsg )

        ( CloseModal, _ ) ->
            ( model, Cmd.none, RequestCloseModal )

        _ ->
            ( model, Cmd.none, NoOutMsg )



-- EFFECTS


addWebhook : AppContext -> ProjectRef -> ProjectWebhookForm -> Cmd Msg
addWebhook appContext projectRef webhook =
    ShareApi.createProjectWebhook projectRef webhook
        |> HttpApi.toRequest (Decode.field "webhook" ProjectWebhook.decode)
            AddWebhookFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewUser : UserSummaryWithId -> Html msg
viewUser user =
    ProfileSnippet.profileSnippet user |> ProfileSnippet.view


divider : Html msg
divider =
    Divider.divider |> Divider.small |> Divider.view


viewtopicselection : List NotificationTopicType -> Html Msg
viewtopicselection selected =
    let
        isSelected topic =
            List.member topic selected

        checkbox title topic =
            CheckboxField.field title (ToggleTopic topic) (isSelected topic)
                |> CheckboxField.view

        contributiontopics =
            [ checkbox "Contribution created" ProjectContributionCreated
            , checkbox "Contribution updated" ProjectContributionUpdated
            , checkbox "Contribution comment" ProjectContributionComment
            ]

        tickettopics =
            [ checkbox "Ticket created" ProjectTicketCreated
            , checkbox "Ticket updated" ProjectTicketUpdated
            , checkbox "Ticket comment" ProjectTicketComment
            ]

        topicselectionCheckboxes =
            div [ class "topic-selection_groups" ]
                [ div []
                    [ div [ class "checkboxes" ]
                        [ checkbox "Branch updated" ProjectBranchUpdated
                        , checkbox "Release created" ProjectReleaseCreated
                        ]
                    ]
                , div []
                    [ div [ class "checkboxes" ] contributiontopics
                    ]
                , div []
                    [ div [ class "checkboxes" ] tickettopics
                    ]
                ]
    in
    div [ class "topic-selection" ] [ divider, topicselectionCheckboxes ]


viewEditModal : Validation -> (Html Msg -> Modal.Modal Msg) -> Form -> Modal.Modal Msg
viewEditModal validation toModal form =
    let
        specifictopicsOption =
            case form.topics of
                AllTopics ->
                    SelectedTopics []

                _ ->
                    form.topics

        options =
            NEL.singleton (RadioField.option "Select specific topics" "The webhook is only called on selected topics" specifictopicsOption)
                |> NEL.cons (RadioField.option "All topics" "The webhook is called on all project topics (including future additions)" AllTopics)

        topicselection =
            case form.topics of
                AllTopics ->
                    UI.nothing

                SelectedTopics selected ->
                    viewtopicselection selected

        withInvalidIndicator t =
            if validation == InvalidUrlAndSelection || validation == InvalidUrl then
                TextField.markAsInvalid t

            else
                t

        withInvalidBanner m =
            if validation == InvalidUrlAndSelection then
                Modal.withLeftSideFooter
                    [ StatusBanner.bad "Please a URL and select topics." ]
                    m

            else if validation == InvalidSelection then
                Modal.withLeftSideFooter
                    [ StatusBanner.bad "Please select topics." ]
                    m

            else
                m
    in
    toModal
        (div []
            [ TextField.field UpdateUrl "Webhook URL" form.url
                |> TextField.withIcon Icon.wireframeGlobe
                |> TextField.withPlaceholder "https://example.com"
                |> TextField.withHelpText "Provide the *full* URL that will be called when the selected topics are triggered."
                |> withInvalidIndicator
                |> TextField.view
            , divider
            , RadioField.field "topics" SetProjectWebhookTopics options form.topics |> RadioField.view
            , topicselection
            ]
        )
        |> withInvalidBanner
        |> Modal.withActions
            [ Button.button CloseModal "Cancel"
                |> Button.subdued
            , Button.button AddWebhook "Add Webhook"
                |> Button.emphasized
            ]



{-
   |> Modal.withLeftSideFooter
       [ Button.iconThenLabel_ Link.docs Icon.docs "Webhook request format docs"
           |> Button.small
           |> Button.outlined
           |> Button.view
       ]
-}


view : Model -> Html Msg
view model =
    let
        modal_ c =
            Modal.content c
                |> Modal.modal "add-project-webhook-modal" CloseModal
                |> Modal.withHeader "Add Webhook"

        modal =
            case model of
                Edit validation form ->
                    viewEditModal validation modal_ form

                Saving f ->
                    viewEditModal NotChecked modal_ f
                        |> Modal.withLeftSideFooter [ StatusBanner.working "Adding Webhook..." ]
                        |> Modal.withDimOverlay True

                Failure _ f ->
                    viewEditModal NotChecked modal_ f
                        |> Modal.withLeftSideFooter [ StatusBanner.bad "Failed to add Webhook" ]

                Success webhook ->
                    let
                        form =
                            { url = Url.toString webhook.url, topics = webhook.topics }
                    in
                    viewEditModal NotChecked modal_ form
                        |> Modal.withLeftSideFooter [ StatusBanner.good "Successfully added Webhook" ]
                        |> Modal.withDimOverlay True
    in
    Modal.view modal
