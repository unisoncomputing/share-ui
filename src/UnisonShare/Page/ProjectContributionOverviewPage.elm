module UnisonShare.Page.ProjectContributionOverviewPage exposing (..)

import Html exposing (Html, div, em, header, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.StatusBanner as StatusBanner
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Account as Account
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution as Contribution exposing (ContributionDetails, ContributionStateToken)
import UnisonShare.Contribution.ContributionEvent as ContributionEvent
import UnisonShare.Contribution.ContributionMergeability as ContributionMergeability exposing (ContributionMergeability)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.ContributionTimeline as ContributionTimeline
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Link as Link
import UnisonShare.Markdown as Markdown
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.Timeline.TimelineEvent as TimelineEvent



-- MODEL


type UpdateStatus
    = TimelineNotReady
    | Idle
    | UpdatingStatus
    | UpdateStatusFailed Http.Error


type MergeStatus
    = Checking
    | Checked ContributionMergeability
    | Merging -- The "Merged" status itself is tracked via UpdateStatus
    | MergeFailed Http.Error
    | Merged
    | CheckFailed Http.Error


type alias Model =
    { timeline : ContributionTimeline.Model
    , updateStatus : UpdateStatus
    , mergeStatus : MergeStatus
    }


init : AppContext -> ProjectRef -> ContributionRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    let
        ( timeline, timelineCmd ) =
            ContributionTimeline.init appContext projectRef contribRef
    in
    ( { timeline = timeline
      , updateStatus = Idle
      , mergeStatus = Checking
      }
    , Cmd.batch
        [ Cmd.map ContributionTimelineMsg timelineCmd
        , fetchMergeability appContext projectRef contribRef
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateStatus ContributionStatus
    | UpdateStatusFinished ContributionStatus (HttpResult ())
    | FetchMergeabilityFinished (HttpResult ContributionMergeability)
    | Merge
    | MergeFinished (HttpResult ())
    | ContributionTimelineMsg ContributionTimeline.Msg


type OutMsg
    = NoOut
    | ContributionStatusUpdated { old : ContributionStatus, new : ContributionStatus }


update : AppContext -> ProjectRef -> ContributionRef -> WebData ContributionDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef contributionRef contribution msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoOut )

        FetchMergeabilityFinished (Ok mergeability) ->
            ( { model | mergeStatus = Checked mergeability }, Cmd.none, NoOut )

        FetchMergeabilityFinished (Err e) ->
            ( { model | mergeStatus = CheckFailed e }, Cmd.none, NoOut )

        Merge ->
            case contribution of
                Success { contributionStateToken } ->
                    ( { model | mergeStatus = Merging }
                    , mergeContribution
                        appContext
                        projectRef
                        contributionRef
                        contributionStateToken
                    , NoOut
                    )

                _ ->
                    ( model, Cmd.none, NoOut )

        MergeFinished resp ->
            case ( appContext.session, contribution, resp ) of
                ( Session.SignedIn me, Success contrib, Ok _ ) ->
                    let
                        contributionEvent =
                            ContributionEvent.StatusChange
                                { newStatus = ContributionStatus.Merged
                                , oldStatus = Just contrib.status
                                , timestamp = appContext.now
                                , actor = Account.toUserSummary me
                                }
                    in
                    ( { model
                        | timeline = ContributionTimeline.addEvent model.timeline contributionEvent
                        , updateStatus = Idle
                        , mergeStatus = Merged
                      }
                    , Cmd.none
                    , ContributionStatusUpdated { old = contrib.status, new = ContributionStatus.Merged }
                    )

                ( _, _, Err e ) ->
                    ( { model | mergeStatus = MergeFailed e }, Cmd.none, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

        UpdateStatus newStatus ->
            ( { model | updateStatus = UpdatingStatus }
            , updateContributionStatus appContext projectRef contributionRef newStatus
            , NoOut
            )

        UpdateStatusFinished newStatus res ->
            case appContext.session of
                Session.SignedIn me ->
                    case ( res, contribution ) of
                        ( Ok _, Success contrib ) ->
                            let
                                contributionEvent =
                                    ContributionEvent.StatusChange
                                        { newStatus = newStatus
                                        , oldStatus = Just contrib.status
                                        , timestamp = appContext.now
                                        , actor = Account.toUserSummary me
                                        }
                            in
                            ( { model
                                | timeline = ContributionTimeline.addEvent model.timeline contributionEvent
                                , updateStatus = Idle
                              }
                            , Cmd.none
                            , ContributionStatusUpdated { old = contrib.status, new = newStatus }
                            )

                        ( Err e, _ ) ->
                            ( { model | updateStatus = UpdateStatusFailed e }, Cmd.none, NoOut )

                        _ ->
                            ( model, Cmd.none, NoOut )

                Session.Anonymous ->
                    ( model, Cmd.none, NoOut )

        ContributionTimelineMsg timelineMsg ->
            let
                ( timeline, timelineCmd ) =
                    ContributionTimeline.update appContext projectRef contributionRef timelineMsg model.timeline
            in
            ( { model | timeline = timeline }, Cmd.map ContributionTimelineMsg timelineCmd, NoOut )



-- EFFECTS


updateContributionStatus :
    AppContext
    -> ProjectRef
    -> ContributionRef
    -> ContributionStatus
    -> Cmd Msg
updateContributionStatus appContext projectRef contributionRef newStatus =
    let
        update_ =
            ShareApi.ProjectContributionStatusUpdate newStatus
    in
    ShareApi.updateProjectContribution projectRef contributionRef update_
        |> HttpApi.toRequestWithEmptyResponse (UpdateStatusFinished newStatus)
        |> HttpApi.perform appContext.api


fetchMergeability : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchMergeability appContext projectRef contributionRef =
    let
        decode =
            Decode.at [ "mergeability", "kind" ] ContributionMergeability.decode
    in
    ShareApi.projectContributionCheckMergeability projectRef contributionRef
        |> HttpApi.toRequest decode FetchMergeabilityFinished
        |> HttpApi.perform appContext.api


mergeContribution : AppContext -> ProjectRef -> ContributionRef -> ContributionStateToken -> Cmd Msg
mergeContribution appContext projectRef contributionRef token =
    ShareApi.projectContributionMerge projectRef contributionRef token
        |> HttpApi.toRequestWithEmptyResponse MergeFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewContribution : Session -> ProjectDetails -> UpdateStatus -> ContributionDetails -> MergeStatus -> Html Msg
viewContribution session project updateStatus contribution mergeStatus =
    let
        isContributor =
            contribution.author
                |> Maybe.map .handle
                |> Maybe.map (\h -> Session.isHandle h session)
                |> Maybe.withDefault False

        canMaintain =
            Project.canMaintain project

        description =
            contribution.description
                |> Maybe.map Markdown.view
                |> Maybe.withDefault (em [ class "no-description" ] [ text "No description..." ])

        archiveButton =
            if (canMaintain || isContributor) && updateStatus /= TimelineNotReady then
                Button.iconThenLabel (UpdateStatus ContributionStatus.Archived) Icon.archive "Archive"
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        mergeButton =
            if canMaintain && updateStatus /= TimelineNotReady then
                case mergeStatus of
                    Checking ->
                        StatusBanner.working "Checking mergeability..."

                    Checked m ->
                        if ContributionMergeability.isMergeable m then
                            Button.iconThenLabel Merge Icon.merge "Merge Contribution"
                                |> Button.positive
                                |> Button.view

                        else
                            let
                                tooltip =
                                    Tooltip.tooltip (Tooltip.text "This Contribution can't be fast-forwarded, please bring it up to date with the target branch.")
                                        |> Tooltip.withArrow Tooltip.End
                            in
                            Tooltip.view
                                (Button.iconThenLabel Merge Icon.merge "Can't be merged"
                                    |> Button.disabled
                                    |> Button.view
                                )
                                tooltip

                    Merging ->
                        StatusBanner.working "Merging..."

                    Merged ->
                        UI.nothing

                    MergeFailed _ ->
                        let
                            tooltip =
                                Tooltip.tooltip (Tooltip.text "Something went wrong when checking if the Contribution could be merged. Please try again.")
                                    |> Tooltip.withArrow Tooltip.End
                        in
                        Tooltip.view
                            (Button.iconThenLabel Merge Icon.warn "Unknown mergeability"
                                |> Button.disabled
                                |> Button.view
                            )
                            tooltip

                    CheckFailed _ ->
                        let
                            tooltip =
                                Tooltip.tooltip (Tooltip.text "Something went wrong when checking if the Contribution could be merged. Please try again.")
                                    |> Tooltip.withArrow Tooltip.End
                        in
                        Tooltip.view
                            (Button.iconThenLabel Merge Icon.warn "Unknown mergeability"
                                |> Button.disabled
                                |> Button.view
                            )
                            tooltip

            else
                UI.nothing

        reopenButton =
            if canMaintain || isContributor then
                Button.iconThenLabel (UpdateStatus ContributionStatus.InReview) Icon.conversation "Re-open"
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        actions =
            case contribution.status of
                ContributionStatus.Draft ->
                    [ div [ class "left-actions" ] []
                    , div [ class "right-actions" ]
                        [ Button.iconThenLabel (UpdateStatus ContributionStatus.InReview) Icon.conversation "Submit for review"
                            |> Button.emphasized
                            |> Button.view
                        ]
                    ]

                ContributionStatus.InReview ->
                    [ div [ class "left-actions" ] []
                    , div [ class "right-actions" ] [ archiveButton, mergeButton ]
                    ]

                ContributionStatus.Merged ->
                    [ div [ class "left-actions" ] []
                    , div [ class "right-actions" ] [ StatusBanner.good "Merged" ]
                    ]

                ContributionStatus.Archived ->
                    [ div [ class "left-actions" ] []
                    , div [ class "right-actions" ] [ reopenButton ]
                    ]

        actions_ =
            if List.isEmpty actions then
                UI.nothing

            else
                div [ class "actions" ] actions

        actionsClassName =
            if updateStatus == UpdatingStatus then
                "contribution-actions contribution-actions_updating"

            else
                "contribution-actions"
    in
    div [ class "contribution-overview_main-cards" ]
        [ Card.card [ description ]
            |> Card.withClassName "contribution-description"
            |> Card.asContained
            |> Card.view
        , Card.card [ actions_ ]
            |> Card.withClassName actionsClassName
            |> Card.asContained
            |> Card.view
        ]


viewStatusChangeEvent : DateTimeContext a -> ContributionEvent.StatusChangeDetails -> List (Html Msg)
viewStatusChangeEvent dtContext { newStatus, oldStatus, actor, timestamp } =
    let
        byAt =
            ByAt.byAt actor timestamp
                |> ByAt.view dtContext.timeZone dtContext.now
    in
    case newStatus of
        ContributionStatus.Draft ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.writingPad
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Created Draft" ]
                    , byAt
                    ]
                ]
            ]

        ContributionStatus.InReview ->
            let
                title =
                    case oldStatus of
                        Just ContributionStatus.Archived ->
                            "Re-opened"

                        _ ->
                            "Submitted for review"
            in
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.conversation
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle title ]
                    , byAt
                    ]
                ]
            ]

        ContributionStatus.Merged ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.merge
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Merged" ]
                    , byAt
                    ]
                ]
            ]

        ContributionStatus.Archived ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.archive
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Archived" ]
                    , byAt
                    ]
                ]
            ]


viewPageContent :
    AppContext
    -> ProjectDetails
    -> UpdateStatus
    -> ContributionDetails
    -> MergeStatus
    -> ContributionTimeline.Model
    -> PageContent Msg
viewPageContent appContext project updateStatus contribution mergeStatus timeline =
    let
        timeline_ =
            ContributionTimeline.view appContext project.ref timeline

        tabs =
            -- Before this date, we couldn't show diffs on merged
            -- contributions, so we don't want to show the "changes" tab
            if DateTime.isAfter Contribution.dateOfHistoricDiffSupport contribution.createdAt || contribution.status == ContributionStatus.InReview then
                TabList.tabList
                    []
                    (TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref))
                    [ TabList.tab "Changes" (Link.projectContributionChanges project.ref contribution.ref) ]
                    |> TabList.view

            else
                UI.nothing
    in
    PageContent.oneColumn
        [ tabs
        , div [ class "project-contribution-overview-page" ]
            [ viewContribution
                appContext.session
                project
                updateStatus
                contribution
                mergeStatus
            , Html.map ContributionTimelineMsg timeline_
            ]
        ]


view : AppContext -> ProjectDetails -> ContributionDetails -> Model -> ( PageContent Msg, Maybe (Html Msg) )
view appContext project contribution model =
    ( viewPageContent
        appContext
        project
        model.updateStatus
        contribution
        model.mergeStatus
        model.timeline
    , Nothing
    )
