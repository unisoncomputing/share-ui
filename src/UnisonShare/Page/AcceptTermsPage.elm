{- This page is used as an interstitial for users signing via UCM or Cloud -}


module UnisonShare.Page.AcceptTermsPage exposing (..)

import Browser.Navigation exposing (load)
import Html exposing (div)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Markdown as Markdown
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Tour as Tour
import Url exposing (Url)



-- MODEL


type alias Model =
    WebData ()


init : Model
init =
    NotAsked



-- UPDATE


type Msg
    = AcceptTerms
    | AcceptTermsFinished (WebData ())
    | Redirect


update : AppContext -> Msg -> Maybe Url -> Model -> ( Model, Cmd Msg )
update appContext msg continueUrl model =
    case msg of
        AcceptTerms ->
            ( model, acceptTerms appContext )

        AcceptTermsFinished r ->
            ( r, Util.delayMsg 1000 Redirect )

        Redirect ->
            case continueUrl of
                Just url ->
                    ( model, load (Url.toString url) )

                Nothing ->
                    -- TODO: Maybe show a message indicating they can go back to ucm (or cloud, or where ever they came from)?
                    ( model, Cmd.none )



-- EFFECTS


acceptTerms : AppContext -> Cmd Msg
acceptTerms appContext =
    ShareApi.completeTours [ Tour.WelcomeTerms ]
        |> HttpApi.toRequestWithEmptyResponse
            (RemoteData.fromResult >> AcceptTermsFinished)
        |> HttpApi.perform appContext.api



-- VIEW


view : Model -> AppDocument Msg
view model =
    let
        acceptButton =
            Button.iconThenLabel AcceptTerms Icon.checkmark "Accept Terms of Service and continue"
                |> Button.positive
                |> Button.medium
                |> Button.view

        ( status, acceptButton_ ) =
            case model of
                NotAsked ->
                    ( UI.nothing, acceptButton )

                Loading ->
                    ( StatusIndicator.view StatusIndicator.working, acceptButton )

                Success _ ->
                    ( StatusBanner.good "Successfully accepted terms, redirecting...", UI.nothing )

                Failure _ ->
                    ( StatusBanner.bad "Something broke on our end and we couldn't accept the terms, please try again", acceptButton )

        content =
            Card.card
                [ div [ class "definition-doc" ]
                    [ Markdown.view_ [] "require:src/terms-of-service.md" ]
                ]
                |> Card.asContained
                |> Card.view

        footer =
            div [ class "accept" ]
                [ div [ class "status" ] [ status ]
                , acceptButton_
                ]

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ content, footer ]
                    |> PageContent.withPageTitle
                        (PageTitle.title "Accept the Unison Terms of Service to continue"
                            |> PageTitle.withIcon Icon.documentCertificate
                        )
                )
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
    in
    { pageId = "accept-terms-page"
    , title = "Unison Terms of Service"
    , appHeader = AppHeader.empty
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
