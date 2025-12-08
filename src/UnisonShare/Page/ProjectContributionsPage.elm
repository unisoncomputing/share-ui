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
import UnisonShare.Contribution as Contribution exposing (ContributionSummary)
import UnisonShare.Contribution.ContributionRef as ContributionRef
import UnisonShare.Contribution.ContributionStatus as ContributionStatus
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectContributionFormModal as ProjectContributionFormModal
import UnisonShare.Route exposing (ProjectContributionsRoute(..))
import UnisonShare.Session as Session exposing (Session)



-- MODEL


type ContribitionsModal
    = NoModal
    | SubmitContributionModal ProjectContributionFormModal.Model


type alias Contributions =
    WebData (List ContributionSummary)


type Tab
    = InReview Contributions
    | Merged Contributions
    | Archived Contributions


type alias RecentBranches =
    { ownContributorBranches : WebData (List BranchSummary)
    , projectBranches : WebData (List BranchSummary)
    }


type alias Model =
    { modal : ContribitionsModal
    , tab : Tab
    , recentBranches : Maybe RecentBranches
    }


init : AppContext -> ProjectRef -> ProjectContributionsRoute -> ( Model, Cmd Msg )
init appContext projectRef subRoute =
    let
        ( tab, statusToLoad ) =
            case subRoute of
                ProjectContributionsInReview ->
                    ( InReview Loading, ContributionStatus.InReview )

                ProjectContributionsMerged ->
                    ( Merged Loading, ContributionStatus.Merged )

                ProjectContributionsArchived ->
                    ( Archived Loading, ContributionStatus.Archived )

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
                            , cursor = Paginated.NoPageCursor
                            }
                      , fetchBranches FetchProjectBranchesFinished
                            appContext
                            projectRef
                            { kind = ShareApi.ProjectBranches
                            , searchQuery = Nothing
                            , limit = 3
                            , cursor = Paginated.NoPageCursor
                            }
                      ]
                    )

                Session.Anonymous ->
                    ( Nothing, [] )
    in
    ( { modal = NoModal
      , tab = tab
      , recentBranches = recentBranches
      }
    , Cmd.batch
        (fetchProjectContributions appContext projectRef statusToLoad :: recentBranchesCmds)
    )



-- UPDATE


type Msg
    = FetchContributionsFinished ContributionStatus.ContributionStatus (WebData (List ContributionSummary))
    | FetchOwnContributorBranchesFinished (WebData (List BranchSummary))
    | FetchProjectBranchesFinished (WebData (List BranchSummary))
    | ShowSubmitContributionModal
    | ProjectContributionFormModalMsg ProjectContributionFormModal.Msg
    | CloseModal


type OutMsg
    = NoOut
    | AddedContribution


update : AppContext -> ProjectRef -> ProjectContributionsRoute -> WebData ProjectDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef _ project msg model =
    case msg of
        FetchContributionsFinished status contributions ->
            let
                tab =
                    case ( status, model.tab ) of
                        ( ContributionStatus.InReview, InReview _ ) ->
                            InReview contributions

                        ( ContributionStatus.Merged, Merged _ ) ->
                            Merged contributions

                        ( ContributionStatus.Archived, Archived _ ) ->
                            Archived contributions

                        _ ->
                            model.tab
            in
            ( { model | tab = tab }, Cmd.none, NoOut )

        FetchOwnContributorBranchesFinished contribBranches ->
            let
                recentBranches =
                    model.recentBranches
                        |> Maybe.map (\rb -> { rb | ownContributorBranches = contribBranches })
            in
            ( { model | recentBranches = recentBranches }, Cmd.none, NoOut )

        FetchProjectBranchesFinished projectBranches ->
            let
                recentBranches =
                    model.recentBranches
                        |> Maybe.map (\rb -> { rb | projectBranches = projectBranches })
            in
            ( { model | recentBranches = recentBranches }, Cmd.none, NoOut )

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
                    , NoOut
                    )

                Session.Anonymous ->
                    ( model, Cmd.none, NoOut )

        ProjectContributionFormModalMsg formMsg ->
            case ( appContext.session, model.modal, project ) of
                ( Session.SignedIn account, SubmitContributionModal formModel, Success p ) ->
                    let
                        ( projectContributionFormModal, cmd, out ) =
                            ProjectContributionFormModal.update appContext p account formMsg formModel

                        ( modal, tab, out_ ) =
                            case ( out, model.tab ) of
                                ( ProjectContributionFormModal.NoOut, _ ) ->
                                    ( SubmitContributionModal projectContributionFormModal, model.tab, NoOut )

                                ( ProjectContributionFormModal.RequestToCloseModal, _ ) ->
                                    ( NoModal, model.tab, NoOut )

                                ( ProjectContributionFormModal.Saved c, InReview contributions ) ->
                                    ( NoModal
                                    , InReview (RemoteData.map (\cs -> c :: cs) contributions)
                                    , AddedContribution
                                    )

                                ( ProjectContributionFormModal.Saved _, _ ) ->
                                    ( NoModal
                                    , model.tab
                                    , AddedContribution
                                    )
                    in
                    ( { model | modal = modal, tab = tab }
                    , Cmd.map ProjectContributionFormModalMsg cmd
                    , out_
                    )

                _ ->
                    ( model, Cmd.none, NoOut )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )


updateSubPage : AppContext -> ProjectRef -> ProjectContributionsRoute -> Model -> ( Model, Cmd Msg )
updateSubPage appContext projectRef subRoute model =
    case subRoute of
        ProjectContributionsInReview ->
            case model.tab of
                InReview _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | tab = InReview Loading }, fetchProjectContributions appContext projectRef ContributionStatus.InReview )

        ProjectContributionsMerged ->
            case model.tab of
                Merged _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | tab = Merged Loading }, fetchProjectContributions appContext projectRef ContributionStatus.Merged )

        ProjectContributionsArchived ->
            case model.tab of
                Archived _ ->
                    ( model, Cmd.none )

                _ ->
                    ( { model | tab = Archived Loading }, fetchProjectContributions appContext projectRef ContributionStatus.Archived )



-- EFFECTS


fetchProjectContributions : AppContext -> ProjectRef -> ContributionStatus.ContributionStatus -> Cmd Msg
fetchProjectContributions appContext projectRef status =
    ShareApi.projectContributions projectRef status
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list Contribution.decodeSummary))
            (RemoteData.fromResult >> FetchContributionsFinished status)
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


viewPageTitle : Session -> ProjectDetails -> Maybe RecentBranches -> PageTitle.PageTitle Msg
viewPageTitle session project recentBranches =
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
                |> Button.emphasized

        canContribute =
            Session.isSignedIn session && (Project.canContribute project || hasRecentContributorBranches)
    in
    if canContribute then
        PageTitle.withRightSide [ Button.view button ] pt

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


viewLoadingPage : Tab -> ProjectRef -> PageLayout Msg
viewLoadingPage tab projectRef =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        button =
            Button.iconThenLabel ShowSubmitContributionModal Icon.merge "Submit contribution"

        content =
            PageContent.oneColumn
                [ tabList projectRef tab |> TabList.view
                , Card.card
                    [ shape Placeholder.Large
                    , shape Placeholder.Small
                    , shape Placeholder.Medium
                    ]
                    |> Card.asContained
                    |> Card.view
                ]
                |> PageContent.withPageTitle
                    (PageTitle.title "Contributions"
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
                    )
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewContributionRow : AppContext -> ProjectRef -> ContributionSummary -> Html Msg
viewContributionRow appContext projectRef contribution =
    let
        byAt =
            case contribution.author of
                Just a ->
                    ByAt.byAt a contribution.createdAt

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
            [ byAt
                |> ByAt.withToClick Link.userProfile
                |> ByAt.view appContext.timeZone appContext.now
            , Tag.view (BranchRef.toTag contribution.sourceBranchRef)
            ]
        ]


tabList : ProjectRef -> Tab -> TabList.TabList msg
tabList projectRef tab =
    case tab of
        InReview _ ->
            TabList.tabList []
                (TabList.tab "In Review" (Link.projectContributions projectRef))
                [ TabList.tab "Merged" (Link.projectContributionsMerged projectRef)
                , TabList.tab "Archived" (Link.projectContributionsArchived projectRef)
                ]

        Merged _ ->
            TabList.tabList
                [ TabList.tab "In Review" (Link.projectContributions projectRef) ]
                (TabList.tab "Merged" (Link.projectContributionsMerged projectRef))
                [ TabList.tab "Archived" (Link.projectContributionsArchived projectRef) ]

        Archived _ ->
            TabList.tabList
                [ TabList.tab "In Review" (Link.projectContributions projectRef)
                , TabList.tab "Merged" (Link.projectContributionsMerged projectRef)
                ]
                (TabList.tab "Archived" (Link.projectContributionsArchived projectRef))
                []


viewPageContent : AppContext -> ProjectDetails -> Maybe RecentBranches -> Tab -> List ContributionSummary -> PageContent Msg
viewPageContent appContext project recentBranches tab contributions =
    let
        viewEmptyState icon text_ =
            EmptyState.iconCloud
                (EmptyState.CircleCenterPiece
                    (div [ class "contributions-empty-state_icon" ] [ Icon.view icon ])
                )
                |> EmptyState.withContent [ h2 [] [ text text_ ] ]
                |> EmptyStateCard.view

        emptyState =
            case tab of
                InReview _ ->
                    viewEmptyState Icon.conversation "There are currently no open contributions in review."

                Merged _ ->
                    viewEmptyState Icon.merge "There are currently no merged contributions."

                Archived _ ->
                    viewEmptyState Icon.archive "There are currently no archived contributions."

        divider =
            Divider.divider
                |> Divider.small
                |> Divider.withoutMargin

        content =
            contributions
                |> List.map (viewContributionRow appContext project.ref)
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
    PageContent.oneColumn [ tabList project.ref tab |> TabList.view, card ]
        |> PageContent.withPageTitle (viewPageTitle appContext.session project recentBranches)


view : AppContext -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    let
        contributions =
            case model.tab of
                InReview contribs ->
                    contribs

                Merged contribs ->
                    contribs

                Archived contribs ->
                    contribs
    in
    case contributions of
        NotAsked ->
            ( viewLoadingPage model.tab project.ref, Nothing )

        Loading ->
            ( viewLoadingPage model.tab project.ref, Nothing )

        Success contribs ->
            let
                modal =
                    case ( model.modal, model.recentBranches ) of
                        ( SubmitContributionModal form, Just _ ) ->
                            Just
                                (Html.map ProjectContributionFormModalMsg
                                    (ProjectContributionFormModal.view project.ref "Submit contribution for review" form)
                                )

                        _ ->
                            Nothing
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext project model.recentBranches model.tab contribs)
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
