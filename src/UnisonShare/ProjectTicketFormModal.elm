module UnisonShare.ProjectTicketFormModal exposing
    ( FormAction(..)
    , Model
    , Msg
    , OutMsg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, section)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button as Button
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.StatusBanner as StatusBanner
import UI.StatusMessage as StatusMessage
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket as Ticket exposing (Ticket)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)



-- MODEL


type Validity
    = NotChecked
    | Valid
    | Invalid
        { needsTitle : Bool
        , needsDescription : Bool
        }


type alias Form =
    { title : String
    , description : String
    , save : WebData Ticket
    , validity : Validity
    }


type FormAction
    = Edit Ticket
    | Create


type alias Model =
    { form : Form
    , action : FormAction
    }


init : FormAction -> Model
init action =
    let
        form =
            case action of
                Edit ticket ->
                    { title = ticket.title
                    , description = ticket.description
                    , save = NotAsked
                    , validity = NotChecked
                    }

                Create ->
                    { title = ""
                    , description = ""
                    , save = NotAsked
                    , validity = NotChecked
                    }
    in
    { form = form, action = action }



-- UPDATE


type Msg
    = CloseModal
    | UpdateSubmissionTitle String
    | UpdateSubmissionDescription String
    | SaveTicket
    | SaveTicketFinished (WebData Ticket)
    | SuccessfullySaved Ticket


type OutMsg
    = None
    | RequestToCloseModal
    | Saved Ticket


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    let
        form =
            model.form
    in
    case ( msg, model.action ) of
        ( CloseModal, _ ) ->
            ( model, Cmd.none, RequestToCloseModal )

        ( UpdateSubmissionTitle t, _ ) ->
            let
                newForm =
                    { form | title = t }
            in
            ( { model | form = newForm }, Cmd.none, None )

        ( UpdateSubmissionDescription d, _ ) ->
            let
                newForm =
                    { form | description = d }
            in
            ( { model | form = newForm }, Cmd.none, None )

        ( SaveTicket, Create ) ->
            let
                validity =
                    case ( form.title, form.description ) of
                        ( "", "" ) ->
                            Invalid { needsTitle = True, needsDescription = True }

                        ( "", _ ) ->
                            Invalid { needsTitle = True, needsDescription = False }

                        ( _, "" ) ->
                            Invalid { needsTitle = False, needsDescription = True }

                        _ ->
                            Valid
            in
            case validity of
                Valid ->
                    if not (String.isEmpty form.title) then
                        let
                            newForm =
                                { form | save = Loading }

                            newTicket =
                                { title = form.title
                                , description = form.description
                                , status = TicketStatus.Open
                                }
                        in
                        ( { model | form = newForm }
                        , createProjectTicket appContext projectRef newTicket
                        , None
                        )

                    else
                        ( model, Cmd.none, None )

                _ ->
                    ( { model | form = { form | validity = validity } }, Cmd.none, None )

        ( SaveTicket, Edit c ) ->
            let
                newForm =
                    { form | save = Loading }

                updatedTicket =
                    { title = form.title
                    , description = form.description
                    , status = c.status
                    }
            in
            ( { model | form = newForm }
            , updateProjectTicket appContext
                projectRef
                c.ref
                updatedTicket
            , None
            )

        ( SaveTicketFinished ticket, _ ) ->
            let
                cmd =
                    case ticket of
                        Success t ->
                            Util.delayMsg 1500 (SuccessfullySaved t)

                        _ ->
                            Cmd.none
            in
            ( { model | form = { form | save = ticket } }, cmd, None )

        ( SuccessfullySaved ticket, _ ) ->
            ( model, Cmd.none, Saved ticket )



-- EFFECTS


createProjectTicket : AppContext -> ProjectRef -> ShareApi.NewProjectTicket -> Cmd Msg
createProjectTicket appContext projectRef newTicket =
    ShareApi.createProjectTicket projectRef newTicket
        |> HttpApi.toRequest
            Ticket.decode
            (RemoteData.fromResult >> SaveTicketFinished)
        |> HttpApi.perform appContext.api


updateProjectTicket :
    AppContext
    -> ProjectRef
    -> TicketRef
    ->
        { title : String
        , description : String
        , status : TicketStatus
        }
    -> Cmd Msg
updateProjectTicket appContext projectRef ticketRef updates =
    ShareApi.updateProjectTicket projectRef ticketRef (ShareApi.ProjectTicketUpdate updates)
        |> HttpApi.toRequest
            Ticket.decode
            (RemoteData.fromResult >> SaveTicketFinished)
        |> HttpApi.perform appContext.api



-- VIEW


view : String -> Model -> Html Msg
view title model =
    let
        form =
            model.form

        titleValidity tf =
            case form.validity of
                Invalid { needsTitle } ->
                    if needsTitle then
                        TextField.markAsInvalid tf

                    else
                        tf

                _ ->
                    tf

        descriptionValidity tf =
            case form.validity of
                Invalid { needsDescription } ->
                    if needsDescription then
                        TextField.markAsInvalid tf

                    else
                        tf

                _ ->
                    tf

        viewForm =
            div [ class "form" ]
                [ section [ class "fields" ]
                    [ TextField.field UpdateSubmissionTitle "Title" form.title
                        |> TextField.withAutofocus
                        |> TextField.withHelpText "Required. Ex. 'Bug in List.map'"
                        |> titleValidity
                        |> TextField.view
                    , TextField.field UpdateSubmissionDescription "Description" form.description
                        |> TextField.withHelpText "Provide a detailed description of your feedback."
                        |> TextField.withRows 8
                        |> descriptionValidity
                        |> TextField.view
                    ]
                ]

        ( content, leftSideFooter, dimOverlay ) =
            case form.save of
                NotAsked ->
                    ( viewForm, [], False )

                Loading ->
                    ( viewForm, [ StatusBanner.working "Saving.." ], True )

                Success ticket ->
                    let
                        status =
                            case model.action of
                                Create ->
                                    StatusMessage.good
                                        ("Successfully created ticket " ++ TicketRef.toString ticket.ref)
                                        []

                                Edit _ ->
                                    StatusMessage.good
                                        ("Successfully updated ticket " ++ TicketRef.toString ticket.ref)
                                        []
                    in
                    ( div [ class "saved" ] [ StatusMessage.view status ], [], False )

                Failure _ ->
                    ( viewForm, [ StatusBanner.bad "Couldn't save, please try again." ], False )
    in
    content
        |> Modal.content
        |> Modal.modal "project-ticket-form-modal" CloseModal
        |> Modal.withActionsIf
            [ Button.button CloseModal "Cancel"
                |> Button.medium
                |> Button.subdued
            , Button.iconThenLabel SaveTicket Icon.rocket title
                |> Button.medium
                |> Button.positive
            ]
            (not (RemoteData.isSuccess form.save))
        |> Modal.withLeftSideFooter leftSideFooter
        |> Modal.withDimOverlay dimOverlay
        |> Modal.withHeaderIf "Save ticket" (not (RemoteData.isSuccess form.save))
        |> Modal.view
