module UnisonShare.Page.ProjectContributionOverviewPage exposing (..)

import Code.BranchRef as BranchRef
import Html exposing (Html, div, em, header, p, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Markdown
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.CopyField as CopyField
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.TabList as TabList
import UI.Tooltip as Tooltip
import UnisonShare.Account as Account
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution as Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionEvent as ContributionEvent
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus exposing (ContributionStatus(..))
import UnisonShare.ContributionTimeline as ContributionTimeline
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Link as Link
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.Timeline.TimelineEvent as TimelineEvent



-- MODEL


type UpdateStatus
    = TimelineNotReady
    | Idle
    | UpdatingStatus
    | UpdateStatusFailed Http.Error


type ContributionOverviewModal
    = NoModal
    | HowToReviewModal


type alias Model =
    { timeline : ContributionTimeline.Model
    , updateStatus : UpdateStatus
    , modal : ContributionOverviewModal
    }


init : AppContext -> ProjectRef -> ContributionRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    let
        ( timeline, timelineCmd ) =
            ContributionTimeline.init appContext projectRef contribRef
    in
    ( { timeline = timeline
      , updateStatus = Idle
      , modal = NoModal
      }
    , Cmd.map ContributionTimelineMsg timelineCmd
    )



-- UPDATE


type Msg
    = NoOp
    | UpdateStatus ContributionStatus
    | UpdateStatusFinished ContributionStatus (HttpResult ())
    | ShowHowToReviewModal
    | CloseModal
    | ContributionTimelineMsg ContributionTimeline.Msg


type OutMsg
    = NoOut
    | ContributionStatusUpdated ContributionStatus


update : AppContext -> ProjectRef -> ContributionRef -> WebData Contribution -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef contributionRef contribution msg model =
    case msg of
        NoOp ->
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
                            , ContributionStatusUpdated newStatus
                            )

                        ( Err e, _ ) ->
                            ( { model | updateStatus = UpdateStatusFailed e }, Cmd.none, NoOut )

                        _ ->
                            ( model, Cmd.none, NoOut )

                Session.Anonymous ->
                    ( model, Cmd.none, NoOut )

        ShowHowToReviewModal ->
            ( { model | modal = HowToReviewModal }, Cmd.none, NoOut )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )

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



-- VIEW


viewContribution : Session -> ProjectRef -> UpdateStatus -> Contribution -> Html Msg
viewContribution session projectRef updateStatus contribution =
    let
        isContributor =
            contribution.authorHandle
                |> Maybe.map (\h -> Session.isHandle h session)
                |> Maybe.withDefault False

        hasProjectAccess =
            Session.hasProjectAccess projectRef session

        className =
            if updateStatus == UpdatingStatus then
                "contribution-description contribution-description_updating"

            else
                "contribution-description"

        description =
            contribution.description
                |> Maybe.map (Markdown.toHtml [ class "definition-doc" ])
                |> Maybe.withDefault (em [ class "no-description" ] [ text "No description..." ])

        browseButton =
            Button.iconThenLabel_
                (Link.projectBranchRoot contribution.projectRef contribution.sourceBranchRef)
                Icon.browse
                "Browse Code"
                |> Button.view

        reviewButton =
            Button.iconThenLabel
                ShowHowToReviewModal
                Icon.questionmark
                "How to review contribution code?"
                |> Button.subdued
                |> Button.small
                |> Button.view

        archiveButton =
            if (hasProjectAccess || isContributor) && updateStatus /= TimelineNotReady then
                Button.iconThenLabel (UpdateStatus Archived) Icon.archive "Archive"
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        mergeButton =
            if hasProjectAccess && updateStatus /= TimelineNotReady then
                let
                    markAsMergedTooltip =
                        Tooltip.tooltip (Tooltip.text "We currently don't support automatic merging.\nPlease merge manually before marking the contribution as merged.")
                            |> Tooltip.withArrow Tooltip.End
                in
                Tooltip.view
                    (Button.iconThenLabel (UpdateStatus Merged) Icon.merge "Mark as Merged"
                        |> Button.positive
                        |> Button.view
                    )
                    markAsMergedTooltip

            else
                UI.nothing

        reopenButton =
            if hasProjectAccess || isContributor then
                Button.iconThenLabel (UpdateStatus InReview) Icon.conversation "Re-open"
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        actions =
            case contribution.status of
                Draft ->
                    [ browseButton
                    , div [ class "right-actions" ]
                        [ Button.iconThenLabel (UpdateStatus InReview) Icon.conversation "Submit for review"
                            |> Button.emphasized
                            |> Button.view
                        ]
                    ]

                InReview ->
                    [ div [ class "left-actions" ] [ browseButton, reviewButton ]
                    , div [ class "right-actions" ] [ archiveButton, mergeButton ]
                    ]

                Merged ->
                    [ browseButton ]

                Archived ->
                    [ browseButton
                    , div [ class "right-actions" ] [ reopenButton ]
                    ]

        actions_ =
            if List.isEmpty actions then
                UI.nothing

            else
                div [ class "actions" ] actions
    in
    Card.card
        [ description, actions_ ]
        |> Card.asContained
        |> Card.withClassName className
        |> Card.view


viewStatusChangeEvent : DateTimeContext a -> ContributionEvent.StatusChangeDetails -> List (Html Msg)
viewStatusChangeEvent dtContext { newStatus, oldStatus, actor, timestamp } =
    let
        byAt =
            ByAt.byAt actor timestamp
                |> ByAt.view dtContext.timeZone dtContext.now
    in
    case newStatus of
        Draft ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.writingPad
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Created Draft" ]
                    , byAt
                    ]
                ]
            ]

        InReview ->
            let
                title =
                    case oldStatus of
                        Just Archived ->
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

        Merged ->
            [ header [ class "timeline-event_header" ]
                [ div [ class "timeline-event_header_description" ]
                    [ TimelineEvent.viewIcon Icon.merge
                    , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Merged" ]
                    , byAt
                    ]
                ]
            ]

        Archived ->
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
    -> ProjectRef
    -> UpdateStatus
    -> Contribution
    -> ContributionTimeline.Model
    -> PageContent Msg
viewPageContent appContext projectRef updateStatus contribution timeline =
    let
        timeline_ =
            ContributionTimeline.view appContext projectRef timeline

        tabs =
            -- Before this date, we couldn't show diffs on merged
            -- contributions, so we don't want to show the "changes" tab
            if DateTime.isAfter Contribution.dateOfHistoricDiffSupport contribution.createdAt || contribution.status == InReview then
                TabList.tabList
                    []
                    (TabList.tab "Overview" (Link.projectContribution projectRef contribution.ref))
                    [ TabList.tab "Changes" (Link.projectContributionChanges projectRef contribution.ref) ]
                    |> TabList.view

            else
                UI.nothing
    in
    PageContent.oneColumn
        [ tabs
        , div [ class "project-contribution-overview-page" ]
            [ viewContribution appContext.session projectRef updateStatus contribution
            , Html.map ContributionTimelineMsg timeline_
            ]
        ]


viewHowToReviewModal : Contribution -> Html Msg
viewHowToReviewModal contribution =
    let
        projectRef =
            ProjectRef.toString contribution.projectRef

        source =
            "/" ++ BranchRef.toString contribution.sourceBranchRef

        target =
            "/" ++ BranchRef.toString contribution.targetBranchRef

        content =
            div []
                [ p [] [ text "Reviewing and merging contribution code is a manual process for now. Follow the steps below." ]
                , div [ class "instructions" ]
                    [ p [] [ text "From within the project clone the source branch:" ]
                    , CopyField.copyField (always NoOp) ("clone " ++ source)
                        |> CopyField.withPrefix (projectRef ++ "/main>")
                        |> CopyField.view
                    , p [] [ text "Next, switch to the target branch:" ]
                    , CopyField.copyField (always NoOp) ("switch " ++ target)
                        |> CopyField.withPrefix (projectRef ++ source ++ ">")
                        |> CopyField.view
                    , p [] [ text "Merge the changes to accept the contribution:" ]
                    , CopyField.copyField (always NoOp) ("merge " ++ source)
                        |> CopyField.withPrefix (projectRef ++ target ++ ">")
                        |> CopyField.view
                    , p [] [ text "Finally, push the project to share and mark the contribution as merged." ]
                    ]
                ]
    in
    content
        |> Modal.content
        |> Modal.modal "project-contribution-how-to-review-modal" CloseModal
        |> Modal.withActions
            [ Button.button CloseModal "Got it"
                |> Button.medium
                |> Button.emphasized
            ]
        |> Modal.withHeader "How to review contribution code?"
        |> Modal.view


view : AppContext -> ProjectRef -> Contribution -> Model -> ( PageContent Msg, Maybe (Html Msg) )
view appContext projectRef contribution model =
    let
        modal =
            case model.modal of
                HowToReviewModal ->
                    Just (viewHowToReviewModal contribution)

                _ ->
                    Nothing
    in
    ( viewPageContent
        appContext
        projectRef
        model.updateStatus
        contribution
        model.timeline
    , modal
    )
