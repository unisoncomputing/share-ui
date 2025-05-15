module UnisonShare.NewOrgModal exposing (..)

import Html exposing (Html, div, form)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import String.Normalize as StringN
import UI.Button as Button
import UI.Form.RadioField as RadioField
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UnisonShare.Account exposing (AccountSummary)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Org as Org exposing (OrgDetails)


type OrgType
    = PublicOrg
    | CommercialOrg


type OrgHandle
    = Blank
    | Suggested UserHandle
    | UserEntered String
    | CheckingAvailability { handle : UserHandle, isSuggested : Bool }
    | NotAvailable { handle : UserHandle, isSuggested : Bool }
    | Available { handle : UserHandle, isSuggested : Bool }


type Validity
    = NotCheckedValidity
    | Valid
    | Invalid { needsName : Bool, needsHandle : Bool }


type alias Model =
    { name : String
    , potentialHandle : OrgHandle
    , orgType : OrgType
    , validity : Validity
    , save : WebData OrgDetails
    }


init : Model
init =
    { name = ""
    , potentialHandle = Blank
    , orgType = PublicOrg
    , validity = NotCheckedValidity
    , save = NotAsked
    }



-- UPDATE


type Msg
    = CloseModal
    | UpdateName String
    | UpdateHandle String
    | UpdateOrgType OrgType
    | CheckHandleAvailability UserHandle
    | HandleAvailabilityCheckFinished UserHandle Bool
    | Save
    | SaveFinished (HttpResult OrgDetails)


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | AddedOrg OrgDetails


update : AppContext -> AccountSummary -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext account msg model =
    case msg of
        UpdateName name ->
            let
                suggestedHandle =
                    toSuggestedHandle name

                suggestAndCheck h isSuggested =
                    if isSuggested then
                        ( Suggested h
                        , Util.delayMsg 250 (CheckHandleAvailability h)
                        )

                    else
                        ( model.potentialHandle, Cmd.none )

                ( potentialHandle, checkCmd ) =
                    case ( suggestedHandle, model.potentialHandle ) of
                        ( Just h, Blank ) ->
                            suggestAndCheck h True

                        ( Just h, Suggested _ ) ->
                            suggestAndCheck h True

                        ( Just h, NotAvailable { isSuggested } ) ->
                            suggestAndCheck h isSuggested

                        ( Just h, Available { isSuggested } ) ->
                            suggestAndCheck h isSuggested

                        _ ->
                            ( model.potentialHandle, Cmd.none )
            in
            ( { model
                | validity = NotCheckedValidity
                , name = name
                , potentialHandle = potentialHandle
              }
            , checkCmd
            , NoOutMsg
            )

        UpdateHandle handleInput ->
            let
                cmd =
                    handleInput
                        |> String.replace "@" ""
                        |> UserHandle.fromUnprefixedString
                        |> Maybe.map (\h -> Util.delayMsg 250 (CheckHandleAvailability h))
                        |> Maybe.withDefault Cmd.none

                potentialHandle =
                    if String.isEmpty handleInput then
                        Blank

                    else
                        UserEntered handleInput
            in
            ( { model
                | validity = NotCheckedValidity
                , potentialHandle = potentialHandle
              }
            , cmd
            , NoOutMsg
            )

        UpdateOrgType orgType ->
            ( { model | validity = NotCheckedValidity, orgType = orgType }, Cmd.none, NoOutMsg )

        CheckHandleAvailability handle ->
            let
                ( model_, cmd ) =
                    case model.potentialHandle of
                        Suggested handle_ ->
                            if UserHandle.equals handle handle_ then
                                ( { model | potentialHandle = CheckingAvailability { handle = handle_, isSuggested = True } }
                                , checkHandleAvailability appContext handle_
                                )

                            else
                                ( model, Cmd.none )

                        UserEntered rawHandle ->
                            case (String.replace "@" "" >> UserHandle.fromUnprefixedString) rawHandle of
                                Just h_ ->
                                    if UserHandle.equals handle h_ then
                                        ( { model | potentialHandle = CheckingAvailability { handle = h_, isSuggested = False } }
                                        , checkHandleAvailability appContext h_
                                        )

                                    else
                                        ( model, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( model_, cmd, NoOutMsg )

        HandleAvailabilityCheckFinished handle isAvailable ->
            let
                handle_ =
                    case model.potentialHandle of
                        CheckingAvailability checking ->
                            if handle == checking.handle then
                                if isAvailable then
                                    Available checking

                                else
                                    NotAvailable checking

                            else
                                model.potentialHandle

                        _ ->
                            model.potentialHandle
            in
            ( { model | potentialHandle = handle_ }, Cmd.none, NoOutMsg )

        Save ->
            let
                model_ =
                    validateForm model
            in
            case ( model_.validity, UserHandle.fromUnprefixedString (handleToString model_.potentialHandle) ) of
                ( Valid, Just handle ) ->
                    ( { model_ | save = Loading }
                    , saveOrg appContext account model.name handle model.orgType
                    , NoOutMsg
                    )

                _ ->
                    ( model_, Cmd.none, NoOutMsg )

        SaveFinished res ->
            case res of
                Ok org ->
                    ( { model | save = Success org }, Util.delayMsg 1500 CloseModal, NoOutMsg )

                Err e ->
                    ( { model | save = Failure e }, Cmd.none, NoOutMsg )

        CloseModal ->
            ( model, Cmd.none, RequestCloseModal )


toSuggestedHandle : String -> Maybe UserHandle
toSuggestedHandle s =
    s |> StringN.slug |> UserHandle.fromUnprefixedString


isSuggestedHandle : OrgHandle -> Bool
isSuggestedHandle h =
    case h of
        Suggested _ ->
            True

        _ ->
            False


validateForm : Model -> Model
validateForm model =
    let
        handle =
            UserHandle.fromUnprefixedString
                (handleToString model.potentialHandle)

        validity =
            case ( not (String.isEmpty model.name), handle ) of
                ( True, Just _ ) ->
                    Valid

                ( True, Nothing ) ->
                    Invalid { needsName = False, needsHandle = True }

                ( False, Just _ ) ->
                    Invalid { needsName = True, needsHandle = False }

                ( False, Nothing ) ->
                    Invalid { needsName = True, needsHandle = True }
    in
    { model | validity = validity }



-- EFFECTS


checkHandleAvailability : AppContext -> UserHandle -> Cmd Msg
checkHandleAvailability appContext h =
    let
        isAvailable res =
            case res of
                Err (Http.BadStatus 404) ->
                    True

                _ ->
                    False
    in
    ShareApi.profile h
        |> HttpApi.toRequest
            (Decode.succeed False)
            (isAvailable >> HandleAvailabilityCheckFinished h)
        |> HttpApi.perform appContext.api


saveOrg : AppContext -> AccountSummary -> String -> UserHandle -> OrgType -> Cmd Msg
saveOrg appContext account name handle orgType =
    ShareApi.createOrg account name handle (orgType == CommercialOrg)
        |> HttpApi.toRequest Org.decodeDetails SaveFinished
        |> HttpApi.perform appContext.api



-- VIEW


handleToString : OrgHandle -> String
handleToString orgHandle =
    case orgHandle of
        Blank ->
            ""

        Suggested h ->
            UserHandle.toUnprefixedString h

        UserEntered h ->
            h

        CheckingAvailability { handle } ->
            UserHandle.toUnprefixedString handle

        NotAvailable { handle } ->
            UserHandle.toUnprefixedString handle

        Available { handle } ->
            UserHandle.toUnprefixedString handle


view : Model -> Html Msg
view model =
    let
        orgTypeOptions =
            RadioField.options2
                (RadioField.option "Public Org"
                    "Only allows public projects."
                    PublicOrg
                )
                (RadioField.option "Commercial Org"
                    "Supports both public and private projects. Selecting this will open a support ticket to enable private projects."
                    CommercialOrg
                )

        handleField =
            TextField.fieldWithoutLabel UpdateHandle "Handle, e.g. @unison" (handleToString model.potentialHandle)
                |> TextField.withHelpText "The unique identifier of the organization and used in URLs and project references like @unison/base."
                |> TextField.withIcon Icon.at

        handleField_ =
            case model.potentialHandle of
                CheckingAvailability _ ->
                    handleField
                        |> TextField.withStatusIndicator StatusIndicator.working

                Available _ ->
                    handleField
                        |> TextField.withStatusIndicator StatusIndicator.good

                NotAvailable _ ->
                    handleField
                        |> TextField.withHelpText "This handle is currently taken by another user or organization."
                        |> TextField.withStatusIndicator StatusIndicator.bad
                        |> TextField.markAsInvalid

                _ ->
                    let
                        h =
                            handleToString model.potentialHandle
                    in
                    if UserHandle.isValidHandle h || String.length h <= 2 then
                        handleField

                    else
                        handleField
                            |> TextField.withHelpText
                                "May only contain alphanumeric characters or hyphens. Can't have multiple consecutive hyphens. Can't begin or end with a hyphen. Max 39 characters."
                            |> TextField.markAsInvalid

        content =
            div []
                [ form []
                    [ TextField.fieldWithoutLabel UpdateName "Name, e.g. Unison" model.name
                        |> TextField.withIcon Icon.tag
                        |> TextField.withAutofocus
                        |> TextField.view
                    , TextField.view handleField_
                    , RadioField.field "org-type" UpdateOrgType orgTypeOptions model.orgType |> RadioField.view
                    ]
                ]

        modal =
            Modal.content content
                |> Modal.modal "new-org-modal" CloseModal
                |> Modal.withHeader "New Organization"
                |> Modal.withActions
                    [ Button.button CloseModal "Cancel"
                        |> Button.subdued
                    , Button.button Save "Create Org"
                        |> Button.positive
                    ]

        modal_ =
            case model.save of
                NotAsked ->
                    case model.validity of
                        Invalid _ ->
                            Modal.withLeftSideFooter
                                [ StatusBanner.bad "Please provide a name and a handle" ]
                                modal

                        _ ->
                            modal

                Loading ->
                    modal
                        |> Modal.withDimOverlay True
                        |> Modal.withLeftSideFooter
                            [ StatusBanner.working "Saving..." ]

                Success _ ->
                    modal

                Failure _ ->
                    Modal.withLeftSideFooter
                        [ StatusBanner.bad "Error, could not save Org. Try again." ]
                        modal
    in
    Modal.view modal_
