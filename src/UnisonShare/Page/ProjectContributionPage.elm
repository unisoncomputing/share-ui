module UnisonShare.Page.ProjectContributionPage exposing (..)

import Code.BranchRef as BranchRef
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.DateTime as DateTime exposing (DateTime)
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution as Contribution exposing (Contribution)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Link as Link
import UnisonShare.Page.ProjectContributionChangesPage as ProjectContributionChangesPage
import UnisonShare.Page.ProjectContributionOverviewPage as ProjectContributionOverviewPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectContributionFormModal as ProjectContributionFormModal
import UnisonShare.Route exposing (ProjectContributionRoute(..))
import UnisonShare.Session as Session



-- MODEL


type ContributionModal
    = NoModal
    | EditModal ProjectContributionFormModal.Model


type ProjectContributionSubPage
    = Overview ProjectContributionOverviewPage.Model
    | Changes ProjectContributionChangesPage.Model


type alias Model =
    { contribution : WebData Contribution
    , modal : ContributionModal
    , subPage : ProjectContributionSubPage
    }


init : AppContext -> ProjectRef -> ContributionRef -> ProjectContributionRoute -> ( Model, Cmd Msg )
init appContext projectRef contribRef route =
    let
        ( subPage, subPageCmd ) =
            case route of
                ProjectContributionOverview ->
                    let
                        ( overviewPage, overviewPageCmd ) =
                            ProjectContributionOverviewPage.init appContext projectRef contribRef
                    in
                    ( Overview overviewPage, Cmd.map ProjectContributionOverviewPageMsg overviewPageCmd )

                ProjectContributionChanges ->
                    let
                        ( changesPage, changesPageCmd ) =
                            ProjectContributionChangesPage.init appContext projectRef contribRef
                    in
                    ( Changes changesPage, Cmd.map ProjectContributionChangesPageMsg changesPageCmd )
    in
    ( { contribution = Loading
      , modal = NoModal
      , subPage = subPage
      }
    , Cmd.batch
        [ fetchContribution appContext projectRef contribRef
        , subPageCmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchContributionFinished (WebData Contribution)
    | ShowEditModal
    | ProjectContributionFormModalMsg ProjectContributionFormModal.Msg
    | CloseModal
    | ProjectContributionOverviewPageMsg ProjectContributionOverviewPage.Msg
    | ProjectContributionChangesPageMsg ProjectContributionChangesPage.Msg


update :
    AppContext
    -> ProjectRef
    -> ContributionRef
    -> ProjectContributionRoute
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
update appContext projectRef contribRef _ msg model =
    case ( model.subPage, msg ) of
        ( _, NoOp ) ->
            ( model, Cmd.none )

        ( _, FetchContributionFinished contrib ) ->
            ( { model | contribution = contrib }, Cmd.none )

        ( _, ShowEditModal ) ->
            case ( appContext.session, model.contribution ) of
                ( Session.SignedIn a, Success contrib ) ->
                    let
                        ( formModel, formCmd ) =
                            ProjectContributionFormModal.init appContext
                                a
                                projectRef
                                (ProjectContributionFormModal.Edit contrib)
                    in
                    ( { model | modal = EditModal formModel }, Cmd.map ProjectContributionFormModalMsg formCmd )

                _ ->
                    ( model, Cmd.none )

        ( _, ProjectContributionFormModalMsg formMsg ) ->
            case ( appContext.session, model.modal ) of
                ( Session.SignedIn account, EditModal formModel ) ->
                    let
                        ( projectContributionFormModal, cmd, out ) =
                            ProjectContributionFormModal.update appContext
                                projectRef
                                account
                                formMsg
                                formModel

                        ( modal, contribution ) =
                            case out of
                                ProjectContributionFormModal.None ->
                                    ( EditModal projectContributionFormModal, model.contribution )

                                ProjectContributionFormModal.RequestToCloseModal ->
                                    ( NoModal, model.contribution )

                                ProjectContributionFormModal.Saved c ->
                                    -- TODO: also add a ContributionEvent
                                    ( NoModal, Success c )
                    in
                    ( { model | modal = modal, contribution = contribution }
                    , Cmd.map ProjectContributionFormModalMsg cmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( _, CloseModal ) ->
            ( { model | modal = NoModal }, Cmd.none )

        ( Overview overviewPage, ProjectContributionOverviewPageMsg overviewPageMsg ) ->
            let
                ( overviewPage_, overviewPageCmd, outMsg ) =
                    ProjectContributionOverviewPage.update appContext
                        projectRef
                        contribRef
                        model.contribution
                        overviewPageMsg
                        overviewPage

                contrib =
                    case outMsg of
                        ProjectContributionOverviewPage.NoOut ->
                            model.contribution

                        ProjectContributionOverviewPage.ContributionStatusUpdated status ->
                            RemoteData.map (\c -> { c | status = status }) model.contribution
            in
            ( { model
                | contribution = contrib
                , subPage = Overview overviewPage_
              }
            , Cmd.map ProjectContributionOverviewPageMsg overviewPageCmd
            )

        ( Changes changesPage, ProjectContributionChangesPageMsg changesPageMsg ) ->
            let
                ( changesPage_, changesPageCmd ) =
                    ProjectContributionChangesPage.update appContext
                        projectRef
                        contribRef
                        changesPageMsg
                        changesPage
            in
            ( { model | subPage = Changes changesPage_ }
            , Cmd.map ProjectContributionChangesPageMsg changesPageCmd
            )

        _ ->
            ( model, Cmd.none )


updateSubPage : AppContext -> ProjectRef -> ContributionRef -> ProjectContributionRoute -> Model -> ( Model, Cmd Msg )
updateSubPage appContext projectRef contribRef contribRoute model =
    case contribRoute of
        ProjectContributionOverview ->
            case model.subPage of
                Overview _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( overviewPage, overviewPageCmd ) =
                            ProjectContributionOverviewPage.init appContext projectRef contribRef
                    in
                    ( { model | subPage = Overview overviewPage }, Cmd.map ProjectContributionOverviewPageMsg overviewPageCmd )

        ProjectContributionChanges ->
            case model.subPage of
                Changes _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( changesPage, changesPageCmd ) =
                            ProjectContributionChangesPage.init appContext projectRef contribRef
                    in
                    ( { model | subPage = Changes changesPage }, Cmd.map ProjectContributionChangesPageMsg changesPageCmd )



-- EFFECTS


fetchContribution : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchContribution appContext projectRef contributionRef =
    ShareApi.projectContribution projectRef contributionRef
        |> HttpApi.toRequest Contribution.decode (RemoteData.fromResult >> FetchContributionFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewPageContent :
    AppContext
    -> ProjectRef
    -> Contribution
    -> ProjectContributionSubPage
    -> ( PageLayout Msg, Maybe (Html Msg) )
viewPageContent appContext projectRef contribution subPage =
    let
        pageTitle_ =
            detailedPageTitle appContext contribution
    in
    case subPage of
        Overview overview ->
            let
                ( overviewPage, modal ) =
                    ProjectContributionOverviewPage.view appContext projectRef contribution overview

                pageContent =
                    PageContent.map ProjectContributionOverviewPageMsg overviewPage
                        |> PageContent.withPageTitle pageTitle_
            in
            ( PageLayout.centeredNarrowLayout
                pageContent
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Maybe.map (Html.map ProjectContributionOverviewPageMsg) modal
            )

        Changes changes ->
            let
                changesPage =
                    ProjectContributionChangesPage.view appContext projectRef contribution changes

                pageContent =
                    PageContent.map ProjectContributionChangesPageMsg changesPage
                        |> PageContent.withPageTitle pageTitle_
            in
            ( PageLayout.edgeToEdgeLayout
                pageContent
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Nothing
            )


timeAgo : DateTimeContext a -> DateTime -> Html msg
timeAgo dateTimeContext t =
    DateTime.view (DateTime.DistanceFrom dateTimeContext.now) dateTimeContext.timeZone t


detailedPageTitle : AppContext -> Contribution -> PageTitle Msg
detailedPageTitle appContext contribution =
    let
        isContributor =
            contribution.author
                |> Maybe.map .handle
                |> Maybe.map (\h -> Session.isHandle h appContext.session)
                |> Maybe.withDefault False

        editButton =
            if isContributor then
                Button.iconThenLabel ShowEditModal Icon.writingPad "Edit"
                    |> Button.small
                    |> Button.outlined
                    |> Button.view

            else
                UI.nothing

        byAt =
            contribution.author
                |> Maybe.map (\a -> ByAt.byAt a contribution.createdAt)
                |> Maybe.withDefault (ByAt.byUnknown contribution.createdAt)
                |> ByAt.view appContext.timeZone appContext.now
    in
    PageTitle.custom
        [ div [ class "contribution-page-title" ]
            [ div [ class "page-title_pre-title" ]
                [ span [ class "contribution-ref_by-at" ]
                    [ span [ class "contribution-ref" ] [ text (ContributionRef.toString contribution.ref) ]
                    , byAt
                    ]
                , editButton
                ]
            , h1 [] [ text contribution.title ]
            , div [ class "page-title_description" ]
                [ span
                    [ class "from-to" ]
                    [ text "from"
                    , span [ class "branches" ]
                        [ BranchRef.toTag contribution.sourceBranchRef
                            |> Tag.withClick (Link.projectBranchRoot contribution.projectRef contribution.sourceBranchRef)
                            |> Tag.large
                            |> Tag.view
                        , text "to"
                        , BranchRef.toTag contribution.targetBranchRef
                            |> Tag.withClick (Link.projectBranchRoot contribution.projectRef contribution.targetBranchRef)
                            |> Tag.large
                            |> Tag.view
                        ]
                    ]
                ]
            ]
        ]


viewLoadingPage : PageLayout Msg
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
                [ div []
                    [ Card.card
                        [ shape Placeholder.Large
                        , shape Placeholder.Small
                        , shape Placeholder.Medium
                        ]
                        |> Card.asContained
                        |> Card.view
                    ]
                ]
                |> PageContent.withPageTitle (PageTitle.title "Loading")
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewErrorPage : ContributionRef -> Http.Error -> PageLayout Msg
viewErrorPage _ _ =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ Card.card
                [ StatusBanner.bad "Something broke on our end and we couldn't show the contribution. Please try again."
                ]
                |> Card.withClassName "project-contribution_error"
                |> Card.asContained
                |> Card.view
            ]
            |> PageContent.withPageTitle (PageTitle.title "Error")
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


view : AppContext -> ProjectRef -> ContributionRef -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext projectRef contribRef model =
    case model.contribution of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success contribution ->
            let
                ( pageLayout, modal_ ) =
                    viewPageContent
                        appContext
                        projectRef
                        contribution
                        model.subPage

                modal =
                    case model.modal of
                        EditModal form ->
                            Just
                                (Html.map ProjectContributionFormModalMsg
                                    (ProjectContributionFormModal.view
                                        projectRef
                                        "Save Contribution"
                                        form
                                    )
                                )

                        _ ->
                            modal_
            in
            ( pageLayout, modal )

        Failure e ->
            ( viewErrorPage contribRef e, Nothing )
