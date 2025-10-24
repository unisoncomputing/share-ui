module UnisonShare.ProjectWebhookExamplesModal exposing (..)

import Html exposing (Html, div, p, pre, text)
import Html.Attributes exposing (class, classList)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import List.Extra as ListE
import List.Nonempty as NEL exposing (Nonempty)
import RemoteData exposing (RemoteData(..), WebData)
import UI.Button as Button
import UI.Click as Click
import UI.CopyOnClick as CopyOnClick
import UI.Modal as Modal
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.ErrorDetails as ErrorDetails
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectWebhookExample as ProjectWebhookExample exposing (ProjectWebhookExample)
import UnisonShare.Session as Session


type alias LoadedModel =
    { examples : Nonempty ProjectWebhookExample
    , activeTopic : String
    }


type alias Model =
    WebData LoadedModel


init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    ( Loading, fetchWebhookExamples appContext projectRef )



-- UPDATE


type Msg
    = CloseModal
    | FetchWebhookExamplesFinished (HttpResult (Nonempty ProjectWebhookExample))
    | ChangeTopic String


type OutMsg
    = NoOutMsg
    | RequestCloseModal


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update _ _ msg model =
    case msg of
        CloseModal ->
            ( model, Cmd.none, RequestCloseModal )

        ChangeTopic newTopic ->
            case model of
                Success loaded ->
                    ( Success { loaded | activeTopic = newTopic }, Cmd.none, NoOutMsg )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        FetchWebhookExamplesFinished res ->
            case res of
                Ok exs ->
                    ( Success
                        { examples = exs
                        , activeTopic = exs |> NEL.map .topic |> NEL.head
                        }
                    , Cmd.none
                    , NoOutMsg
                    )

                Err e ->
                    ( Failure e, Cmd.none, NoOutMsg )



-- EFFECTS


fetchWebhookExamples : AppContext -> ProjectRef -> Cmd Msg
fetchWebhookExamples appContext _ =
    ShareApi.webhookExamples
        |> HttpApi.toRequest ProjectWebhookExample.decodeList
            FetchWebhookExamplesFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewPayload : String -> Html Msg
viewPayload json =
    div [ class "example-json-format" ]
        [ CopyOnClick.copyButton json
        , pre [] [ text json ]
        ]


viewExamples : LoadedModel -> Html Msg
viewExamples { examples, activeTopic } =
    let
        matchingExample =
            examples
                |> NEL.toList
                |> ListE.find (\e -> e.topic == activeTopic)

        json =
            case matchingExample of
                Just e ->
                    e.payload

                Nothing ->
                    ""

        viewExampleTab e =
            Click.onClick (ChangeTopic e.topic)
                |> Click.view
                    [ class "example-topic"
                    , classList [ ( "active-topic", activeTopic == e.topic ) ]
                    ]
                    [ text e.topic ]
    in
    div [ class "examples" ]
        [ div [ class "examples-list" ]
            (examples |> NEL.map viewExampleTab |> NEL.toList)
        , viewPayload json
        ]


view : Session.Session -> Model -> Html Msg
view session model =
    let
        modal_ c =
            Modal.content c
                |> Modal.modal "project-webhook-examples-modal" CloseModal
                |> Modal.withHeader "Project webhook examples"

        modal =
            case model of
                Success loaded ->
                    modal_
                        (div []
                            [ p [] [ text "Overview of the JSON formats that are POSTed to a configured webhook URL when those events are triggered." ]
                            , viewExamples loaded
                            ]
                        )

                Failure e ->
                    modal_ (div [] [ StatusBanner.bad "Failed to load webhook examples", ErrorDetails.view session e ])

                _ ->
                    let
                        shape length =
                            Placeholder.text
                                |> Placeholder.withLength length
                                |> Placeholder.subdued
                                |> Placeholder.tiny
                                |> Placeholder.view
                    in
                    modal_
                        (div
                            [ class "examples-loading" ]
                            [ shape Placeholder.Large
                            , shape Placeholder.Small
                            , shape Placeholder.Medium
                            , shape Placeholder.Large
                            , shape Placeholder.Small
                            , shape Placeholder.Medium
                            ]
                        )
    in
    modal
        |> Modal.withActions [ Button.button CloseModal "Close" ]
        |> Modal.view
