module UnisonShare.Page.ProjectContributionChecksPage exposing (..)

import Code.BranchRef as BranchRef
import Code.Hash as Hash
import Html exposing (Html, div, header, pre, text)
import Html.Attributes exposing (class)
import Lib.UserHandle as UserHandle
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import Set.Extra as SetE
import Time
import UI
import UI.Card as Card
import UI.DateTime as DateTime exposing (DateTime)
import UI.Divider as Divider
import UI.FoldToggle as FoldToggle
import UI.PageContent as PageContent exposing (PageContent)
import UI.StatusIndicator as StatusIndicator
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Check as Check exposing (Check, CheckId)
import UnisonShare.Contribution exposing (ContributionDetails)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Page.ProjectContributionPageHelpers as ProjectContributionPageHelpers exposing (tabs)
import UnisonShare.Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import Url



-- MODEL


type alias Folds =
    -- Set CheckId
    Set String


type alias Model =
    { checks : WebData (List Check)
    , folds : Folds
    }


init : AppContext -> ProjectRef -> ContributionRef -> Maybe CheckId -> ( Model, Cmd Msg )
init appContext projectRef contribRef _ =
    ( { checks = Loading
      , folds = Set.empty
      }
    , fetchChecks appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = NoOp
    | FetchChecksFinished (WebData (List Check))
    | ToggleFold CheckId


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ msg model =
    case msg of
        FetchChecksFinished checks ->
            ( { model | checks = checks }, Cmd.none )

        ToggleFold checkId ->
            let
                checkId_ =
                    Check.checkIdToString checkId
            in
            ( { model | folds = SetE.toggle checkId_ model.folds }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


fetchChecks : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchChecks _ projectRef _ =
    let
        baseTime =
            Time.millisToPosix 1701619200000

        startedAt =
            DateTime.fromPosix baseTime

        endedAt =
            DateTime.fromPosix (Time.millisToPosix 1701619500000)

        mockHash =
            Hash.unsafeFromString "#abc123def456"

        mockBranchRef =
            BranchRef.main_

        checks =
            [ { id = Check.CheckId "check-1"
              , runner = Check.LocalUcm (UserHandle.unsafeFromString "testuser")
              , projectRef = projectRef
              , branchRef = mockBranchRef
              , causalHash = mockHash
              , createdAt = startedAt
              , updatedAt = endedAt
              , status =
                    Check.Success
                        { startedAt = startedAt
                        , endedAt = endedAt
                        , output = "All tests passed successfully!"
                        }
              }
            , { id = Check.CheckId "check-2"
              , runner =
                    Check.Webhook
                        (Url.fromString "https://example.com/webhook"
                            |> Maybe.withDefault
                                { protocol = Url.Https
                                , host = "example.com"
                                , port_ = Nothing
                                , path = "/webhook"
                                , query = Nothing
                                , fragment = Nothing
                                }
                        )
              , projectRef = projectRef
              , branchRef = mockBranchRef
              , causalHash = mockHash
              , createdAt = startedAt
              , updatedAt = endedAt
              , status =
                    Check.Failure
                        { startedAt = startedAt
                        , endedAt = endedAt
                        , errorTitle = "Type Error"
                        , errorDetails = "Function signature mismatch in main module"
                        , output = "Error at line 42: Expected Int but got String"
                        }
              }
            , { id = Check.CheckId "check-3"
              , runner = Check.LocalUcm (UserHandle.unsafeFromString "anotheruser")
              , projectRef = projectRef
              , branchRef = mockBranchRef
              , causalHash = mockHash
              , createdAt = startedAt
              , updatedAt = DateTime.fromPosix (Time.millisToPosix 1701619300000)
              , status =
                    Check.Waiting
                        { startedAt = startedAt
                        }
              }
            ]
    in
    Util.delayMsg 100 (FetchChecksFinished (Success checks))



{-
   ShareApi.projectContributionChecks projectRef contribRef
       |> HttpApi.toRequest (Decode.field "checks" (Decode.list Check.decode))
           (RemoteData.fromResult >> FetchChecksFinished)
       |> HttpApi.perform appContext.api
-}
-- VIEW


viewTiming : DateTime -> DateTime -> Html msg
viewTiming start end =
    let
        duration =
            DateTime.duration start end

        hours =
            if duration.hours > 0 then
                text (String.fromInt duration.hours ++ "h")

            else
                UI.nothing

        minutes =
            if duration.minutes > 0 then
                text (String.fromInt duration.minutes ++ "m")

            else
                UI.nothing

        seconds =
            if duration.seconds > 0 then
                text (String.fromInt duration.seconds ++ "s")

            else
                UI.nothing
    in
    div [ class "timing" ] [ text "took ", hours, minutes, seconds ]


viewTitle : AppContext -> Folds -> Check -> Html Msg
viewTitle appContext folds check =
    let
        { result, date, foldToggle, timing } =
            case check.status of
                Check.NotStarted ->
                    { result = StatusIndicator.view StatusIndicator.working
                    , date = DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone check.createdAt
                    , timing = UI.nothing
                    , foldToggle = FoldToggle.view FoldToggle.disabled
                    }

                Check.Waiting _ ->
                    { result = StatusIndicator.view StatusIndicator.working
                    , date = DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone check.createdAt
                    , timing = UI.nothing
                    , foldToggle = FoldToggle.view FoldToggle.disabled
                    }

                Check.Timeout { startedAt, endedAt } ->
                    { result = StatusIndicator.view StatusIndicator.bad
                    , date = DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone check.createdAt
                    , timing = viewTiming startedAt endedAt
                    , foldToggle = FoldToggle.view FoldToggle.disabled
                    }

                Check.Failure { startedAt, endedAt } ->
                    { result = StatusIndicator.view StatusIndicator.bad
                    , date = DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone check.createdAt
                    , timing = viewTiming startedAt endedAt
                    , foldToggle =
                        FoldToggle.foldToggle (ToggleFold check.id)
                            |> FoldToggle.isOpen (Set.member (Check.checkIdToString check.id) folds)
                            |> FoldToggle.view
                    }

                Check.Success { startedAt, endedAt } ->
                    { result = StatusIndicator.view StatusIndicator.good
                    , date = DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone check.createdAt
                    , timing = viewTiming startedAt endedAt
                    , foldToggle =
                        FoldToggle.foldToggle (ToggleFold check.id)
                            |> FoldToggle.isOpen (Set.member (Check.checkIdToString check.id) folds)
                            |> FoldToggle.view
                    }

        ( title, subTitle ) =
            case check.runner of
                Check.LocalUcm h ->
                    ( div [ class "check_title" ] [ text "Local check" ]
                    , div [ class "check_sub-title" ] [ text (UserHandle.toString h) ]
                    )

                Check.Webhook url ->
                    ( div [ class "check_title" ] [ text "External check" ]
                    , div [ class "check_sub-title" ] [ text (Url.toString url) ]
                    )

        hash =
            Hash.view check.causalHash
    in
    header [ class "check_header" ]
        [ div [ class "check_header_first-row" ]
            [ div [ class "check_header_left" ]
                [ foldToggle
                , div [ class "result-and-title" ] [ result, title ]
                ]
            , div [ class "check_header_right" ] [ hash, date ]
            ]
        , div [ class "check_header_second-row" ]
            [ subTitle
            , timing
            ]
        ]


viewOutput : String -> Html Msg
viewOutput output =
    pre [ class "check_output" ] [ text output ]


viewExpanded : Folds -> Check -> Html Msg
viewExpanded folds check =
    let
        isUnfolded =
            Set.member (Check.checkIdToString check.id) folds
    in
    case ( check.status, isUnfolded ) of
        ( Check.Success { output }, True ) ->
            div [ class "check_expanded" ] [ viewOutput output ]

        ( Check.Failure { output }, True ) ->
            div [ class "check_expanded" ] [ viewOutput output ]

        _ ->
            UI.nothing


viewLatest : AppContext -> Folds -> Check -> Html Msg
viewLatest appContext folds check =
    let
        content =
            viewExpanded folds check
    in
    Card.card
        [ viewTitle appContext folds check, content ]
        |> Card.asContained
        |> Card.withClassName "checks_latest"
        |> Card.view


viewPrevious : AppContext -> Folds -> List Check -> Html Msg
viewPrevious appContext folds checks =
    let
        divider =
            Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view

        viewPrevCheck c =
            div [ class "check_previous" ]
                [ viewTitle appContext folds c
                , viewExpanded folds c
                ]
    in
    Card.card (checks |> List.map viewPrevCheck |> List.intersperse divider)
        |> Card.asContained
        |> Card.withClassName "checks_previous"
        |> Card.view


view : AppContext -> ProjectDetails -> ContributionDetails -> Model -> PageContent Msg
view appContext project contribution model =
    let
        content =
            case model.checks of
                NotAsked ->
                    [ text "Loading..." ]

                Loading ->
                    [ text "Loading..." ]

                Success checks ->
                    case checks of
                        latest :: [] ->
                            [ viewLatest appContext model.folds latest ]

                        latest :: previous ->
                            [ viewLatest appContext model.folds latest
                            , viewPrevious appContext model.folds previous
                            ]

                        [] ->
                            [ text "todo empty state" ]

                Failure _ ->
                    [ text "Error" ]
    in
    PageContent.oneColumn
        [ tabs ProjectContributionPageHelpers.Checks project contribution
        , div [ class "project-contribution-checks-page" ] content
        ]
