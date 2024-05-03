module UnisonShare.Page.ProjectContributionsPage exposing (..)

import Code.BranchRef as BranchRef
import Html exposing (Html, div, h2, header, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.TabList as TabList
import UI.Tag as Tag
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.Contribution as Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionRef as ContributionRef
import UnisonShare.Contribution.ContributionStatus as ContributionStatus
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectContributionFormModal as ProjectContributionFormModal
import UnisonShare.Session as Session exposing (Session)



-- MODEL


type ContribitionsModal
    = NoModal
    | SubmitContributionModal ProjectContributionFormModal.Model


type Tab
    = InReview
    | Merged
    | Archived


type alias RecentBranches =
    { ownContributorBranches : WebData (List BranchSummary)
    , projectBranches : WebData (List BranchSummary)
    }


type alias Model =
    { contributions : WebData (List Contribution)
    , modal : ContribitionsModal
    , tab : Tab
    , recentBranches : Maybe RecentBranches
    }


init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    let
        ( recentBranches, recentBranchesCmds ) =
            case appContext.session of
                Session.SignedIn a ->
                    ( Just { ownContributorBranches = Loading, projectBranches = Loading }
                    , [ fetchBranches FetchOwnContributorBranchesFinished
                            appContext
                            projectRef
                            { kind = ShareApi.ContributorBranches (Just a.handle)
                            , searchQuery = Nothing
                            , limit = 3
                            , cursor = Nothing
                            }
                      , fetchBranches FetchProjectBranchesFinished
                            appContext
                            projectRef
                            { kind = ShareApi.ProjectBranches
                            , searchQuery = Nothing
                            , limit = 3
                            , cursor = Nothing
                            }
                      ]
                    )

                Session.Anonymous ->
                    ( Nothing, [] )
    in
    ( { contributions = Loading
      , modal = NoModal
      , tab = InReview
      , recentBranches = recentBranches
      }
    , Cmd.batch
        (fetchProjectContributions appContext projectRef :: recentBranchesCmds)
    )



-- UPDATE


type Msg
    = FetchContributionsFinished (WebData (List Contribution))
    | FetchOwnContributorBranchesFinished (WebData (List BranchSummary))
    | FetchProjectBranchesFinished (WebData (List BranchSummary))
    | ShowSubmitContributionModal
    | ProjectContributionFormModalMsg ProjectContributionFormModal.Msg
    | CloseModal
    | ChangeTab Tab


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef msg model =
    case msg of
        FetchContributionsFinished contributions ->
            ( { model | contributions = contributions }, Cmd.none )

        FetchOwnContributorBranchesFinished contribBranches ->
            let
                recentBranches =
                    model.recentBranches
                        |> Maybe.map (\rb -> { rb | ownContributorBranches = contribBranches })
            in
            ( { model | recentBranches = recentBranches }, Cmd.none )

        FetchProjectBranchesFinished projectBranches ->
            let
                recentBranches =
                    model.recentBranches
                        |> Maybe.map (\rb -> { rb | projectBranches = projectBranches })
            in
            ( { model | recentBranches = recentBranches }, Cmd.none )

        ShowSubmitContributionModal ->
            case appContext.session of
                Session.SignedIn a ->
                    let
                        ( projectContributionFormModal, cmd ) =
                            ProjectContributionFormModal.init
                                appContext
                                a
                                projectRef
                                ProjectContributionFormModal.Create
                    in
                    ( { model | modal = SubmitContributionModal projectContributionFormModal }
                    , Cmd.map ProjectContributionFormModalMsg cmd
                    )

                Session.Anonymous ->
                    ( model, Cmd.none )

        ProjectContributionFormModalMsg formMsg ->
            case ( appContext.session, model.modal ) of
                ( Session.SignedIn account, SubmitContributionModal formModel ) ->
                    let
                        ( projectContributionFormModal, cmd, out ) =
                            ProjectContributionFormModal.update appContext projectRef account formMsg formModel

                        ( modal, contributions ) =
                            case out of
                                ProjectContributionFormModal.None ->
                                    ( SubmitContributionModal projectContributionFormModal, model.contributions )

                                ProjectContributionFormModal.RequestToCloseModal ->
                                    ( NoModal, model.contributions )

                                ProjectContributionFormModal.Saved c ->
                                    ( NoModal, RemoteData.map (\cs -> c :: cs) model.contributions )
                    in
                    ( { model | modal = modal, contributions = contributions }
                    , Cmd.map ProjectContributionFormModalMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ChangeTab t ->
            ( { model | tab = t }, Cmd.none )



-- EFFECTS


fetchProjectContributions : AppContext -> ProjectRef -> Cmd Msg
fetchProjectContributions appContext projectRef =
    ShareApi.projectContributions projectRef
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list Contribution.decode))
            (RemoteData.fromResult >> FetchContributionsFinished)
        |> HttpApi.perform appContext.api


fetchBranches :
    (WebData (List BranchSummary) -> Msg)
    -> AppContext
    -> ProjectRef
    -> ShareApi.ProjectBranchesParams
    -> Cmd Msg
fetchBranches doneMsg appContext projectRef params =
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list BranchSummary.decode))
            (RemoteData.fromResult >> doneMsg)
        |> HttpApi.perform appContext.api



-- VIEW


viewPageTitle : Session -> ProjectRef -> Maybe RecentBranches -> PageTitle.PageTitle Msg
viewPageTitle session projectRef recentBranches =
    let
        pt =
            PageTitle.title "Contributions"

        hasRecentContributorBranches =
            MaybeE.unwrap
                False
                (.ownContributorBranches >> RemoteData.map (List.isEmpty >> not) >> RemoteData.withDefault False)
                recentBranches

        button =
            Button.iconThenLabel ShowSubmitContributionModal Icon.merge "Submit contribution"
    in
    if Session.hasProjectAccess projectRef session || hasRecentContributorBranches then
        pt
            |> PageTitle.withRightSide
                [ button
                    |> Button.emphasized
                    |> Button.view
                ]

    else
        pt
            |> PageTitle.withRightSide
                [ div [ class "submit-contribution-disabled" ]
                    [ Tooltip.text "Create a Contribution by pushing a feature branch to Share."
                        |> Tooltip.tooltip
                        |> Tooltip.view
                            (button
                                |> Button.disabled
                                |> Button.view
                            )
                    ]
                ]


viewLoadingPage : PageLayout msg
viewLoadingPage =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        content =
            PageContent.oneColumn
                [ Card.card
                    [ shape Placeholder.Large
                    , shape Placeholder.Small
                    , shape Placeholder.Medium
                    ]
                    |> Card.asContained
                    |> Card.view
                ]
                |> PageContent.withPageTitle (PageTitle.title "Contributions")
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewContributionRow : AppContext -> ProjectRef -> Contribution -> Html Msg
viewContributionRow appContext projectRef contribution =
    let
        byAt =
            case contribution.authorHandle of
                Just h ->
                    ByAt.handleOnly h contribution.createdAt

                Nothing ->
                    ByAt.byUnknown contribution.createdAt

        numComments =
            if contribution.numComments > 0 then
                div [ class "num-comments" ]
                    [ Icon.view Icon.conversation
                    , text (String.fromInt contribution.numComments)
                    ]

            else
                UI.nothing
    in
    div [ class "contribution-row" ]
        [ header [ class "contribution-row_header" ]
            [ Click.view []
                [ h2 []
                    [ span [ class "contribution-row_ref" ]
                        [ text (ContributionRef.toString contribution.ref)
                        ]
                    , text contribution.title
                    ]
                ]
                (Link.projectContribution projectRef contribution.ref)
            , numComments
            ]
        , div [ class "contribution-row_info" ]
            [ ByAt.view appContext.timeZone appContext.now byAt
            , Tag.view (BranchRef.toTag contribution.sourceBranchRef)
            ]
        ]


viewPageContent : AppContext -> ProjectRef -> Maybe RecentBranches -> Tab -> List Contribution -> PageContent Msg
viewPageContent appContext projectRef recentBranches tab contributions =
    let
        viewEmptyState icon text_ =
            EmptyState.iconCloud
                (EmptyState.CircleCenterPiece
                    (div [ class "contributions-empty-state_icon" ] [ Icon.view icon ])
                )
                |> EmptyState.withContent [ h2 [] [ text text_ ] ]
                |> EmptyStateCard.view

        ( tabList, status, emptyState ) =
            case tab of
                InReview ->
                    ( TabList.tabList []
                        (TabList.tab "In Review" (Click.onClick (ChangeTab InReview)))
                        [ TabList.tab "Merged" (Click.onClick (ChangeTab Merged))
                        , TabList.tab "Archived" (Click.onClick (ChangeTab Archived))
                        ]
                    , ContributionStatus.InReview
                    , viewEmptyState Icon.conversation "There are currently no open contributions in review."
                    )

                Merged ->
                    ( TabList.tabList
                        [ TabList.tab "In Review" (Click.onClick (ChangeTab InReview)) ]
                        (TabList.tab "Merged" (Click.onClick (ChangeTab Merged)))
                        [ TabList.tab "Archived" (Click.onClick (ChangeTab Archived)) ]
                    , ContributionStatus.Merged
                    , viewEmptyState Icon.merge "There are currently no merged contributions."
                    )

                Archived ->
                    ( TabList.tabList
                        [ TabList.tab "In Review" (Click.onClick (ChangeTab InReview))
                        , TabList.tab "Merged" (Click.onClick (ChangeTab Merged))
                        ]
                        (TabList.tab "Archived" (Click.onClick (ChangeTab Archived)))
                        []
                    , ContributionStatus.Archived
                    , viewEmptyState Icon.archive "There are currently no archived contributions."
                    )

        divider =
            Divider.divider
                |> Divider.small
                |> Divider.withoutMargin

        content =
            contributions
                |> List.filter (\c -> c.status == status)
                |> List.map (viewContributionRow appContext projectRef)
                |> List.intersperse (Divider.view divider)

        card =
            if List.isEmpty content then
                emptyState

            else
                Card.card content
                    |> Card.withClassName "project-contributions"
                    |> Card.asContained
                    |> Card.view
    in
    PageContent.oneColumn [ TabList.view tabList, card ]
        |> PageContent.withPageTitle (viewPageTitle appContext.session projectRef recentBranches)


view : AppContext -> ProjectRef -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext projectRef model =
    case model.contributions of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success contributions ->
            let
                modal =
                    case ( model.modal, model.recentBranches ) of
                        ( SubmitContributionModal form, Just _ ) ->
                            Just
                                (Html.map ProjectContributionFormModalMsg
                                    (ProjectContributionFormModal.view projectRef "Submit contribution for review" form)
                                )

                        _ ->
                            Nothing
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext projectRef model.recentBranches model.tab contributions)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure _ ->
            -- TODO
            ( PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ text "Couldn't load contributions..." ])
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Nothing
            )
