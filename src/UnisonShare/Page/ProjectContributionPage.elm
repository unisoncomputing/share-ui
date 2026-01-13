module UnisonShare.Page.ProjectContributionPage exposing (..)

import Code.BranchRef as BranchRef
import Html exposing (Html, div, h1, h3, p, span, text)
import Html.Attributes exposing (class)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.ButtonGroup as ButtonGroup
import UI.ByAt as ByAt
import UI.Card as Card
import UI.CopyField as CopyField
import UI.DateTime as DateTime exposing (DateTime)
import UI.Divider as Divider
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution as Contribution exposing (ContributionDetails)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Link as Link
import UnisonShare.Page.ProjectContributionChangesPage as ProjectContributionChangesPage
import UnisonShare.Page.ProjectContributionChecksPage as ProjectContributionChecksPage
import UnisonShare.Page.ProjectContributionOverviewPage as ProjectContributionOverviewPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectContributionFormModal as ProjectContributionFormModal
import UnisonShare.Route exposing (ProjectContributionRoute(..))
import UnisonShare.Session as Session



-- MODEL


type ContributionModal
    = NoModal
    | EditModal ProjectContributionFormModal.Model
    | ViewLocallyInstructionsModal


type ProjectContributionSubPage
    = Overview ProjectContributionOverviewPage.Model
    | Changes ProjectContributionChangesPage.Model
    | Checks ProjectContributionChecksPage.Model


type alias Model =
    { contribution : WebData ContributionDetails
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

                ProjectContributionChanges focus ->
                    let
                        ( changesPage, changesPageCmd ) =
                            ProjectContributionChangesPage.init appContext projectRef contribRef focus
                    in
                    ( Changes changesPage, Cmd.map ProjectContributionChangesPageMsg changesPageCmd )

                ProjectContributionChecks checkId ->
                    let
                        ( checksPage, checksPageCmd ) =
                            ProjectContributionChecksPage.init appContext projectRef contribRef checkId
                    in
                    ( Checks checksPage, Cmd.map ProjectContributionChecksPageMsg checksPageCmd )
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
    | FetchContributionFinished (WebData ContributionDetails)
    | FetchMergeabilityFinished (HttpResult Bool)
    | ShowEditModal
    | ProjectContributionFormModalMsg ProjectContributionFormModal.Msg
    | CloseModal
    | ProjectContributionOverviewPageMsg ProjectContributionOverviewPage.Msg
    | ProjectContributionChangesPageMsg ProjectContributionChangesPage.Msg
    | ProjectContributionChecksPageMsg ProjectContributionChecksPage.Msg
    | ShowViewLocallyInstructionsModal


type OutMsg
    = NoOut
    | ContributionStatusUpdated { old : ContributionStatus, new : ContributionStatus }


update :
    AppContext
    -> ProjectRef
    -> ContributionRef
    -> ProjectContributionRoute
    -> WebData ProjectDetails
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef contribRef _ project msg model =
    case ( model.subPage, msg ) of
        ( _, NoOp ) ->
            ( model, Cmd.none, NoOut )

        ( _, FetchContributionFinished contrib ) ->
            ( { model | contribution = contrib }, Cmd.none, NoOut )

        ( _, ShowEditModal ) ->
            case ( appContext.session, model.contribution ) of
                ( Session.SignedIn a, Success contrib ) ->
                    let
                        ( formModel, formCmd ) =
                            ProjectContributionFormModal.init appContext
                                a
                                projectRef
                                (ProjectContributionFormModal.Edit (Contribution.toSummary contrib))
                    in
                    ( { model | modal = EditModal formModel }, Cmd.map ProjectContributionFormModalMsg formCmd, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

        ( _, ShowViewLocallyInstructionsModal ) ->
            ( { model | modal = ViewLocallyInstructionsModal }, Cmd.none, NoOut )

        ( _, ProjectContributionFormModalMsg formMsg ) ->
            case ( appContext.session, model.contribution ) of
                ( Session.SignedIn account, Success contrib ) ->
                    case ( model.modal, project ) of
                        ( EditModal formModel, Success p ) ->
                            let
                                ( projectContributionFormModal, cmd, out ) =
                                    ProjectContributionFormModal.update appContext
                                        p
                                        account
                                        formMsg
                                        formModel

                                ( modal, contribution ) =
                                    case out of
                                        ProjectContributionFormModal.NoOut ->
                                            ( EditModal projectContributionFormModal, model.contribution )

                                        ProjectContributionFormModal.RequestToCloseModal ->
                                            ( NoModal, model.contribution )

                                        ProjectContributionFormModal.Saved newContrib ->
                                            -- TODO: also add a ContributionEvent
                                            ( NoModal
                                            , Success
                                                (Contribution.toDetails
                                                    contrib.contributionStateToken
                                                    Nothing
                                                    newContrib
                                                )
                                            )
                            in
                            ( { model | modal = modal, contribution = contribution }
                            , Cmd.map ProjectContributionFormModalMsg cmd
                            , NoOut
                            )

                        _ ->
                            ( model, Cmd.none, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

        ( _, CloseModal ) ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )

        ( Overview overviewPage, ProjectContributionOverviewPageMsg overviewPageMsg ) ->
            let
                ( overviewPage_, overviewPageCmd, outMsg ) =
                    ProjectContributionOverviewPage.update appContext
                        projectRef
                        contribRef
                        model.contribution
                        overviewPageMsg
                        overviewPage

                ( contrib, out ) =
                    case outMsg of
                        ProjectContributionOverviewPage.NoOut ->
                            ( model.contribution, NoOut )

                        ProjectContributionOverviewPage.ContributionStatusUpdated status ->
                            ( RemoteData.map (\c -> { c | status = status.new }) model.contribution, ContributionStatusUpdated status )
            in
            ( { model
                | contribution = contrib
                , subPage = Overview overviewPage_
              }
            , Cmd.map ProjectContributionOverviewPageMsg overviewPageCmd
            , out
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
            , NoOut
            )

        ( Checks checksPage, ProjectContributionChecksPageMsg checksPageMsg ) ->
            let
                ( checksPage_, checksPageCmd ) =
                    ProjectContributionChecksPage.update appContext
                        projectRef
                        contribRef
                        checksPageMsg
                        checksPage
            in
            ( { model | subPage = Checks checksPage_ }
            , Cmd.map ProjectContributionChecksPageMsg checksPageCmd
            , NoOut
            )

        _ ->
            ( model, Cmd.none, NoOut )


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

        ProjectContributionChanges focus ->
            case model.subPage of
                Changes _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( changesPage, changesPageCmd ) =
                            ProjectContributionChangesPage.init appContext projectRef contribRef focus
                    in
                    ( { model | subPage = Changes changesPage }, Cmd.map ProjectContributionChangesPageMsg changesPageCmd )

        ProjectContributionChecks checkId ->
            case model.subPage of
                Checks _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( checksPage, checksPageCmd ) =
                            ProjectContributionChecksPage.init appContext projectRef contribRef checkId
                    in
                    ( { model | subPage = Checks checksPage }, Cmd.map ProjectContributionChecksPageMsg checksPageCmd )



-- EFFECTS


fetchContribution : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchContribution appContext projectRef contributionRef =
    ShareApi.projectContribution projectRef contributionRef
        |> HttpApi.toRequest Contribution.decodeDetails (RemoteData.fromResult >> FetchContributionFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewPageContent :
    AppContext
    -> ProjectDetails
    -> ContributionDetails
    -> ProjectContributionSubPage
    -> ( PageLayout Msg, Maybe (Html Msg) )
viewPageContent appContext project contribution subPage =
    let
        pageTitle_ =
            detailedPageTitle appContext contribution
    in
    case subPage of
        Overview overview ->
            let
                ( overviewPage, modal ) =
                    ProjectContributionOverviewPage.view appContext project contribution overview

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
                    ProjectContributionChangesPage.view appContext project.ref contribution changes

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

        Checks checks ->
            let
                checksPage =
                    ProjectContributionChecksPage.view appContext project contribution checks

                pageContent =
                    PageContent.map ProjectContributionChecksPageMsg checksPage
                        |> PageContent.withPageTitle pageTitle_
            in
            ( PageLayout.centeredNarrowLayout
                pageContent
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Nothing
            )


timeAgo : DateTimeContext a -> DateTime -> Html msg
timeAgo dateTimeContext t =
    DateTime.view (DateTime.DistanceFrom dateTimeContext.now) dateTimeContext.timeZone t


detailedPageTitle : AppContext -> ContributionDetails -> PageTitle Msg
detailedPageTitle appContext contribution =
    let
        isContributor =
            contribution.author
                |> Maybe.map .handle
                |> Maybe.map (\h -> Session.isHandle h appContext.session)
                |> Maybe.withDefault False

        buttons =
            ButtonGroup.buttonGroup
                [ Button.iconThenLabel_ (Link.projectBranchRoot contribution.projectRef contribution.sourceBranchRef) Icon.browse "Browse code"
                    |> Button.small
                , Button.iconThenLabel ShowViewLocallyInstructionsModal Icon.download "View locally"
                    |> Button.withDomId "view-locally"
                    |> Button.small
                ]

        editButton =
            if isContributor then
                Button.iconThenLabel ShowEditModal Icon.writingPad "Edit"
                    |> Button.small
                    |> Button.view

            else
                UI.nothing

        byAt =
            contribution.author
                |> Maybe.map (\a -> ByAt.byAt a contribution.createdAt)
                |> Maybe.withDefault (ByAt.byUnknown contribution.createdAt)
                |> ByAt.withToClick Link.userProfile
                |> ByAt.view appContext.timeZone appContext.now
    in
    PageTitle.custom
        [ div [ class "contribution-page-title" ]
            [ div [ class "page-title_pre-title" ]
                [ span [ class "contribution-ref_by-at" ]
                    [ span [ class "contribution-ref" ] [ text (ContributionRef.toString contribution.ref) ]
                    , byAt
                    ]
                , div [ class "contribution-page-title_actions" ]
                    [ editButton
                    , ButtonGroup.view buttons
                    ]
                ]
            , h1 [] [ text contribution.title ]
            , div [ class "page-title_description" ]
                [ span
                    [ class "from-to" ]
                    [ span [ class "branch" ]
                        [ text "Merge from"
                        , ButtonGroup.buttonGroup
                            [ Button.iconThenLabel_ (Link.projectBranchRoot contribution.projectRef contribution.sourceBranchRef)
                                Icon.branch
                                (BranchRef.toString contribution.sourceBranchRef)
                                |> Button.small
                            ]
                            |> ButtonGroup.withCopyButton (BranchRef.toString contribution.sourceBranchRef)
                            |> ButtonGroup.view
                        ]
                    , span [ class "branch" ]
                        [ text "to"
                        , ButtonGroup.buttonGroup
                            [ Button.iconThenLabel_ (Link.projectBranchRoot contribution.projectRef contribution.targetBranchRef)
                                Icon.branch
                                (BranchRef.toString contribution.targetBranchRef)
                                |> Button.small
                            ]
                            |> ButtonGroup.withCopyButton (BranchRef.toString contribution.targetBranchRef)
                            |> ButtonGroup.view
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


viewViewLocallyInstructionsModal : ContributionDetails -> Html Msg
viewViewLocallyInstructionsModal contribution =
    let
        projectRef =
            ProjectRef.toString contribution.projectRef

        source =
            "/" ++ BranchRef.toString contribution.sourceBranchRef

        target =
            "/" ++ BranchRef.toString contribution.targetBranchRef

        mergeInstructions_ =
            [ Divider.divider |> Divider.small |> Divider.view
            , h3 [] [ text "Merge (and resolve conflicts) locally:" ]
            , div [ class "instructions" ]
                [ p [] [ text "Clone the contribution branch:" ]
                , CopyField.copyField (always NoOp) ("clone " ++ source)
                    |> CopyField.withPrefix (projectRef ++ "/main>")
                    |> CopyField.view
                , p [] [ text "Next, switch to the target branch (usually /main):" ]
                , CopyField.copyField (always NoOp) ("switch " ++ target)
                    |> CopyField.withPrefix (projectRef ++ source ++ ">")
                    |> CopyField.view
                , p [] [ text "Make sure the target branch is up to date:" ]
                , CopyField.copyField (always NoOp) "pull"
                    |> CopyField.withPrefix (projectRef ++ "/main>")
                    |> CopyField.view
                , p [] [ text "Merge the changes:" ]
                , CopyField.copyField (always NoOp) ("merge " ++ source)
                    |> CopyField.withPrefix (projectRef ++ target ++ ">")
                    |> CopyField.view
                , p [] [ text "Finally, push the project to share and mark the contribution as merged." ]
                ]
            ]

        mergeInstructions =
            case contribution.status of
                ContributionStatus.Draft ->
                    mergeInstructions_

                ContributionStatus.InReview ->
                    mergeInstructions_

                _ ->
                    []

        content =
            div []
                ([ h3 [] [ text "View the contribution locally:" ]
                 , div [ class "instructions" ]
                    [ p [] [ text "Clone the contribution branch:" ]
                    , CopyField.copyField (always NoOp) ("clone " ++ source)
                        |> CopyField.withPrefix (projectRef ++ "/main>")
                        |> CopyField.view
                    ]
                 ]
                    ++ mergeInstructions
                )
    in
    content
        |> Modal.content
        |> Modal.modal "project-contribution-view-locally-instructions-modal" CloseModal
        |> Modal.withActions
            [ Button.button CloseModal "Got it"
                |> Button.medium
                |> Button.emphasized
            ]
        |> Modal.view


view : AppContext -> ProjectDetails -> ContributionRef -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project contribRef model =
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
                        project
                        contribution
                        model.subPage

                modal =
                    case model.modal of
                        EditModal form ->
                            Just
                                (Html.map ProjectContributionFormModalMsg
                                    (ProjectContributionFormModal.view
                                        project.ref
                                        "Save Contribution"
                                        form
                                    )
                                )

                        ViewLocallyInstructionsModal ->
                            Just (viewViewLocallyInstructionsModal contribution)

                        _ ->
                            modal_
            in
            ( pageLayout, modal )

        Failure e ->
            ( viewErrorPage contribRef e, Nothing )
