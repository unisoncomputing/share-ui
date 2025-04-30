module UnisonShare.Page.FinishSignupPage exposing (..)

import Html exposing (br, footer, h1, p, strong, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import UI.Button as Button
import UI.Card as Card
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.StatusIndicator as StatusIndicator
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter



-- MODEL


type PotentialHandle
    = Blank
    | UserEntered String
    | CheckingAvailability UserHandle
    | NotAvailable UserHandle
    | Available UserHandle


type alias Model =
    { potentialHandle : PotentialHandle }


init : Model
init =
    { potentialHandle = Blank }



-- UPDATE


type Msg
    = NoOp
    | UpdateHandle String
    | CheckHandleAvailability UserHandle
    | HandleAvailabilityCheckFinished UserHandle Bool


update : AppContext -> UserHandle -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ msg model =
    case msg of
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
            ( { model | potentialHandle = potentialHandle }
            , cmd
            )

        CheckHandleAvailability handle ->
            let
                ( model_, cmd ) =
                    case model.potentialHandle of
                        UserEntered rawHandle ->
                            case (String.replace "@" "" >> UserHandle.fromUnprefixedString) rawHandle of
                                Just h_ ->
                                    if UserHandle.equals handle h_ then
                                        ( { model | potentialHandle = CheckingAvailability h_ }
                                        , checkHandleAvailability appContext h_
                                        )

                                    else
                                        ( model, Cmd.none )

                                Nothing ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model, Cmd.none )
            in
            ( model_, cmd )

        HandleAvailabilityCheckFinished handle isAvailable ->
            let
                potentialHandle =
                    case model.potentialHandle of
                        CheckingAvailability handle_ ->
                            if handle == handle_ then
                                if isAvailable then
                                    Available handle_

                                else
                                    NotAvailable handle_

                            else
                                model.potentialHandle

                        _ ->
                            model.potentialHandle
            in
            ( { model | potentialHandle = potentialHandle }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



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



-- VIEW


handleToString : PotentialHandle -> String
handleToString potentialHandle =
    case potentialHandle of
        Blank ->
            ""

        UserEntered raw ->
            raw

        CheckingAvailability handle ->
            UserHandle.toUnprefixedString handle

        NotAvailable handle ->
            UserHandle.toUnprefixedString handle

        Available handle ->
            UserHandle.toUnprefixedString handle


view : AppContext -> UserHandle -> Model -> AppDocument Msg
view appContext conflictingHandle { potentialHandle } =
    let
        handleField =
            TextField.fieldWithoutLabel UpdateHandle "Handle, e.g. @alice" (handleToString potentialHandle)
                |> TextField.withHelpText "The unique identifier of your user used in URLs and project references like @unison/base."
                |> TextField.withIcon Icon.at

        disabledFinishButton =
            Button.button
                NoOp
                "Finish Signup"
                |> Button.positive
                |> Button.disabled
                |> Button.view

        finishButton handle =
            Button.button_
                (Link.finishSignup appContext.api handle)
                "Finish Signup"
                |> Button.positive
                |> Button.view

        ( handleField_, finishButton_ ) =
            case potentialHandle of
                CheckingAvailability _ ->
                    ( handleField
                        |> TextField.withStatusIndicator StatusIndicator.working
                    , disabledFinishButton
                    )

                Available handle ->
                    ( handleField
                        |> TextField.withStatusIndicator StatusIndicator.good
                    , finishButton handle
                    )

                NotAvailable _ ->
                    ( handleField
                        |> TextField.withHelpText "This handle is currently taken by another user or organization."
                        |> TextField.withStatusIndicator StatusIndicator.bad
                        |> TextField.markAsInvalid
                    , disabledFinishButton
                    )

                _ ->
                    let
                        h =
                            handleToString potentialHandle
                    in
                    if UserHandle.isValidHandle h || String.length h <= 2 then
                        ( handleField, disabledFinishButton )

                    else
                        ( handleField
                            |> TextField.withHelpText
                                "May only contain alphanumeric characters or hyphens. Can't have multiple consecutive hyphens. Can't begin or end with a hyphen. Max 39 characters."
                            |> TextField.markAsInvalid
                        , disabledFinishButton
                        )

        content =
            [ Card.card
                [ h1 [] [ text "Almost there..." ]
                , p
                    []
                    [ text "Looks like the handle "
                    , strong [] [ text (UserHandle.toString conflictingHandle) ]
                    , text " is already taken."
                    , br [] []
                    , text "Pick another one to finish signing up to Unison Share."
                    ]
                , TextField.view handleField_
                , footer [ class "actions" ] [ finishButton_ ]
                ]
                |> Card.withClassName "form"
                |> Card.asContained
                |> Card.view
            ]

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn content)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
    in
    { pageId = "finish-signup-page"
    , title = "Finish Signup"
    , appHeader = AppHeader.empty
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
