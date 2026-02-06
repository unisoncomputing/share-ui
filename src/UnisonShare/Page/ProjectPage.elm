module UnisonShare.Page.ProjectPage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Perspective as Perspective
import Code.ProjectSlug as ProjectSlug
import Code.Version as Version exposing (Version)
import Html exposing (Html, br, div, footer, form, h1, h3, p, span, strong, text)
import Html.Attributes exposing (class)
import Http exposing (Error(..))
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.AnchoredOverlay as AnchoredOverlay
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.CopyField as CopyField
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.ErrorCard as ErrorCard
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.Sidebar as Sidebar
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus
import UnisonShare.Page.CodePage as CodePage
import UnisonShare.Page.ProjectBranchesPage as ProjectBranchesPage
import UnisonShare.Page.ProjectContributionPage as ProjectContributionPage
import UnisonShare.Page.ProjectContributionsPage as ProjectContributionsPage
import UnisonShare.Page.ProjectHistoryPage as ProjectHistoryPage
import UnisonShare.Page.ProjectOverviewPage as ProjectOverviewPage
import UnisonShare.Page.ProjectPageHeader as ProjectPageHeader
import UnisonShare.Page.ProjectReleasePage as ProjectReleasePage
import UnisonShare.Page.ProjectReleasesPage as ProjectReleasesPage
import UnisonShare.Page.ProjectSettingsPage as ProjectSettingsPage
import UnisonShare.Page.ProjectTicketPage as ProjectTicketPage
import UnisonShare.Page.ProjectTicketsPage as ProjectTicketsPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Paginated as Paginated
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route exposing (CodeRoute, ProjectRoute(..))
import UnisonShare.Session as Session
import UnisonShare.SwitchBranch as SwitchBranch
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus
import UnisonShare.UcmCommand as UcmCommand



-- MODEL


type SubPage
    = Overview ProjectOverviewPage.Model
    | Branches ProjectBranchesPage.Model
    | Code BranchRef CodePage.Model
    | Release Version ProjectReleasePage.Model
    | Releases ProjectReleasesPage.Model
    | Contribution ContributionRef ProjectContributionPage.Model
    | Contributions ProjectContributionsPage.Model
    | Ticket TicketRef ProjectTicketPage.Model
    | Tickets ProjectTicketsPage.Model
    | History (Maybe BranchRef) Paginated.PageCursorParam ProjectHistoryPage.Model
    | Settings ProjectSettingsPage.Model


type alias DeleteProject =
    { confirmText : String, deleting : WebData () }


type ProjectPageModal
    = NoModal
    | UseProjectModal
    | DeleteProjectModal DeleteProject


type alias Model =
    { subPage : SubPage
    , project : WebData ProjectDetails
    , modal : ProjectPageModal
    , switchBranch : SwitchBranch.Model
    , mobileNavIsOpen : Bool
    }


init : AppContext -> ProjectRef -> ProjectRoute -> ( Model, Cmd Msg )
init appContext projectRef route =
    let
        ( subPage, pageCmd ) =
            case route of
                ProjectOverview ->
                    let
                        ( overviewPage, overviewCmd ) =
                            ProjectOverviewPage.init appContext projectRef
                    in
                    ( Overview overviewPage, Cmd.map ProjectOverviewPageMsg overviewCmd )

                ProjectBranches branchesRoute cursor ->
                    let
                        ( branchesPage, branchesCmd ) =
                            ProjectBranchesPage.init appContext projectRef branchesRoute cursor
                    in
                    ( Branches branchesPage, Cmd.map ProjectBranchesPageMsg branchesCmd )

                ProjectBranch branchRef codeRoute ->
                    let
                        codeBrowsingContext =
                            { projectRef = projectRef, branchRef = branchRef }

                        ( codePage, codePageCmd ) =
                            CodePage.init appContext codeBrowsingContext codeRoute
                    in
                    ( Code branchRef codePage, Cmd.map CodePageMsg codePageCmd )

                ProjectContribution contribRef contribRoute ->
                    let
                        ( contribution_, contribCmd ) =
                            ProjectContributionPage.init
                                appContext
                                projectRef
                                contribRef
                                contribRoute
                    in
                    ( Contribution contribRef contribution_, Cmd.map ProjectContributionPageMsg contribCmd )

                ProjectContributions subRoute ->
                    let
                        ( contributions_, contribsCmd ) =
                            ProjectContributionsPage.init
                                appContext
                                projectRef
                                subRoute
                    in
                    ( Contributions contributions_, Cmd.map ProjectContributionsPageMsg contribsCmd )

                ProjectTicket ticketRef ->
                    let
                        ( ticket_, ticketCmd ) =
                            ProjectTicketPage.init
                                appContext
                                projectRef
                                ticketRef
                    in
                    ( Ticket ticketRef ticket_, Cmd.map ProjectTicketPageMsg ticketCmd )

                ProjectTickets ->
                    let
                        ( tickets_, ticketsCmd ) =
                            ProjectTicketsPage.init appContext projectRef
                    in
                    ( Tickets tickets_, Cmd.map ProjectTicketsPageMsg ticketsCmd )

                ProjectRelease version ->
                    let
                        ( release_, releaseCmd ) =
                            ProjectReleasePage.init
                                appContext
                                projectRef
                                version
                    in
                    ( Release version release_, Cmd.map ProjectReleasePageMsg releaseCmd )

                ProjectReleases ->
                    let
                        ( releases_, releasesCmd ) =
                            ProjectReleasesPage.init
                                appContext
                                projectRef
                                Loading
                    in
                    ( Releases releases_, Cmd.map ProjectReleasesPageMsg releasesCmd )

                ProjectHistory branchRef cursor ->
                    ( History branchRef cursor ProjectHistoryPage.preInit, Cmd.none )

                ProjectSettings ->
                    let
                        settings =
                            ProjectSettingsPage.preInit
                    in
                    ( Settings settings, Cmd.none )
    in
    ( { subPage = subPage
      , project = Loading
      , modal = NoModal
      , switchBranch = SwitchBranch.init
      , mobileNavIsOpen = False
      }
    , Cmd.batch [ fetchProject appContext projectRef, pageCmd ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchProjectFinished (WebData ProjectDetails)
    | ToggleProjectFav
    | SetProjectFavFinished Bool (HttpResult ())
    | ToggleProjectSubscription
    | SetProjectSubscriptionFinished Bool (HttpResult ())
    | ShowUseProjectModal
    | ShowDeleteProjectModal
    | YesDeleteProject
    | UpdateDeleteProjectModalConfirmText String
    | DeleteProjectFinished (HttpResult ())
    | CloseModal
    | ToggleMobileNav
    | SwitchBranchMsg SwitchBranch.Msg
    | ProjectOverviewPageMsg ProjectOverviewPage.Msg
    | CodePageMsg CodePage.Msg
    | ProjectReleasePageMsg ProjectReleasePage.Msg
    | ProjectReleasesPageMsg ProjectReleasesPage.Msg
    | ProjectContributionPageMsg ProjectContributionPage.Msg
    | ProjectContributionsPageMsg ProjectContributionsPage.Msg
    | ProjectTicketPageMsg ProjectTicketPage.Msg
    | ProjectTicketsPageMsg ProjectTicketsPage.Msg
    | ProjectHistoryPageMsg ProjectHistoryPage.Msg
    | ProjectBranchesPageMsg ProjectBranchesPage.Msg
    | ProjectSettingsPageMsg ProjectSettingsPage.Msg
    | ChangeRouteTo Route.Route


update : AppContext -> ProjectRef -> ProjectRoute -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef route msg model =
    let
        codeBrowsingContext br =
            { projectRef = projectRef, branchRef = br }
    in
    case ( model.subPage, msg ) of
        ( _, ChangeRouteTo r ) ->
            ( model, Route.navigate appContext.navKey r )

        ( _, FetchProjectFinished project ) ->
            let
                modelWithProject =
                    { model | project = project }
            in
            case ( model.subPage, project ) of
                ( Releases rm, Success project_ ) ->
                    case project_.latestVersion of
                        Just v ->
                            let
                                ( releases, cmd ) =
                                    ProjectReleasesPage.fetchLatestReleaseNotesAndUpdate
                                        appContext
                                        projectRef
                                        rm
                                        v
                            in
                            ( { modelWithProject | subPage = Releases releases }
                            , Cmd.map ProjectReleasesPageMsg cmd
                            )

                        Nothing ->
                            let
                                releases =
                                    ProjectReleasesPage.updateWithNoLatestReleaseNotes rm
                            in
                            ( { modelWithProject | subPage = Releases releases }
                            , Cmd.none
                            )

                ( Releases rm, _ ) ->
                    let
                        releases =
                            ProjectReleasesPage.updateWithNoLatestReleaseNotes rm
                    in
                    ( { modelWithProject | subPage = Releases releases }
                    , Cmd.none
                    )

                ( Overview ov, Success project_ ) ->
                    let
                        ( overview, cmd ) =
                            ProjectOverviewPage.fetchDependenciesAndUpdate
                                appContext
                                project_
                                ov
                    in
                    ( { modelWithProject | subPage = Overview overview }
                    , Cmd.map ProjectOverviewPageMsg cmd
                    )

                ( Settings settings, Success _ ) ->
                    let
                        ( fetchingCollabs, fetchingCollabsCmd ) =
                            ProjectSettingsPage.fetchProjectCollaborators appContext projectRef settings

                        ( fetchingOwner, fetchingOwnerCmd ) =
                            ProjectSettingsPage.fetchProjectOwner appContext projectRef fetchingCollabs

                        ( fetchingWebhooks, fetchingWebhooksCmd ) =
                            ProjectSettingsPage.fetchProjectWebhooks appContext projectRef fetchingOwner
                    in
                    ( { modelWithProject | subPage = Settings fetchingWebhooks }
                    , Cmd.batch
                        [ Cmd.map ProjectSettingsPageMsg fetchingCollabsCmd
                        , Cmd.map ProjectSettingsPageMsg fetchingOwnerCmd
                        , Cmd.map ProjectSettingsPageMsg fetchingWebhooksCmd
                        ]
                    )

                ( History branchRef cursor _, Success project_ ) ->
                    let
                        ( history, cmd ) =
                            ProjectHistoryPage.init appContext project_ branchRef cursor
                    in
                    ( { modelWithProject | subPage = History branchRef cursor history }
                    , Cmd.map ProjectHistoryPageMsg cmd
                    )

                _ ->
                    ( modelWithProject, Cmd.none )

        ( _, ToggleProjectFav ) ->
            case model.project of
                Success project ->
                    let
                        project_ =
                            Project.toggleFav project
                    in
                    ( { model | project = Success project_ }, setProjectFav appContext project_ )

                _ ->
                    ( model, Cmd.none )

        ( _, SetProjectFavFinished _ isFavedResult ) ->
            case ( model.project, isFavedResult ) of
                -- We have a project, but the API request to faving failed
                ( Success project, Err _ ) ->
                    -- Reverting the fav change by re-toggling Project.isFaved.
                    ( { model | modal = NoModal, project = Success (Project.toggleFav project) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, ToggleProjectSubscription ) ->
            case model.project of
                Success project ->
                    let
                        project_ =
                            Project.toggleSubscription project
                    in
                    ( { model | project = Success project_ }, setProjectSubscription appContext project_ )

                _ ->
                    ( model, Cmd.none )

        ( _, SetProjectSubscriptionFinished _ isSubscribedResult ) ->
            case ( model.project, isSubscribedResult ) of
                -- We have a project, but the API request to subscribing failed
                ( Success project, Err _ ) ->
                    -- Reverting the subscription change by re-toggling Project.isSubscribed.
                    ( { model | modal = NoModal, project = Success (Project.toggleSubscription project) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, ToggleMobileNav ) ->
            ( { model | mobileNavIsOpen = not model.mobileNavIsOpen }, Cmd.none )

        ( _, ShowDeleteProjectModal ) ->
            ( { model | modal = DeleteProjectModal { confirmText = "", deleting = NotAsked } }, Cmd.none )

        ( _, YesDeleteProject ) ->
            case model.modal of
                DeleteProjectModal del ->
                    if del.confirmText == "delete" then
                        ( { model | modal = DeleteProjectModal { del | deleting = Loading } }
                        , deleteProject appContext projectRef
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( _, DeleteProjectFinished res ) ->
            case model.modal of
                DeleteProjectModal del ->
                    let
                        deleting =
                            RemoteData.fromResult res

                        modal =
                            DeleteProjectModal { del | deleting = deleting }

                        cmd =
                            case res of
                                Ok _ ->
                                    Util.delayMsg 1500 (ChangeRouteTo Route.catalog)

                                _ ->
                                    Cmd.none
                    in
                    ( { model | modal = modal }, cmd )

                _ ->
                    ( model, Cmd.none )

        ( _, UpdateDeleteProjectModalConfirmText t ) ->
            case model.modal of
                DeleteProjectModal del ->
                    ( { model | modal = DeleteProjectModal { del | confirmText = t } }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        -- Sub msgs
        ( Overview overviewPage, ProjectOverviewPageMsg overviewPageMsg ) ->
            case route of
                Route.ProjectOverview ->
                    let
                        ( overviewPage_, overviewCmd, out ) =
                            ProjectOverviewPage.update appContext
                                projectRef
                                model.project
                                overviewPageMsg
                                overviewPage

                        ( model_, cmd ) =
                            case ( model.project, out ) of
                                ( Success _, ProjectOverviewPage.RequestToShowUseProjectModal ) ->
                                    ( { model | modal = UseProjectModal }, Cmd.none )

                                ( Success project, ProjectOverviewPage.RequestToToggleProjectFav ) ->
                                    let
                                        project_ =
                                            Project.toggleFav project
                                    in
                                    ( { model | modal = NoModal, project = Success project_ }, setProjectFav appContext project_ )

                                ( Success project, ProjectOverviewPage.RequestToToggleProjectSubscription ) ->
                                    let
                                        project_ =
                                            Project.toggleSubscription project
                                    in
                                    ( { model | modal = NoModal, project = Success project_ }, setProjectSubscription appContext project_ )

                                ( Success project, ProjectOverviewPage.ProjectDescriptionUpdated description ) ->
                                    let
                                        updatedProject =
                                            { project
                                                | summary = description.summary
                                                , tags = description.tags
                                            }
                                    in
                                    ( { model | modal = NoModal, project = Success updatedProject }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )
                    in
                    ( { model_ | subPage = Overview overviewPage_ }
                    , Cmd.batch [ Cmd.map ProjectOverviewPageMsg overviewCmd, cmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        ( Branches branchesPage, ProjectBranchesPageMsg branchesMsg ) ->
            case route of
                Route.ProjectBranches branchesRoute _ ->
                    let
                        ( branchesPage_, branchesCmd ) =
                            ProjectBranchesPage.update appContext projectRef branchesRoute branchesMsg branchesPage
                    in
                    ( { model | subPage = Branches branchesPage_ }
                    , Cmd.map ProjectBranchesPageMsg branchesCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Code branchRef codePage, CodePageMsg codePageMsg ) ->
            case route of
                Route.ProjectBranch _ cr ->
                    let
                        ( codePage_, codePageCmd ) =
                            CodePage.update appContext (codeBrowsingContext branchRef) cr codePageMsg codePage
                    in
                    ( { model | subPage = Code branchRef codePage_ }
                    , Cmd.map CodePageMsg codePageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Release currentVersion releasePage, ProjectReleasePageMsg releasePageMsg ) ->
            case route of
                Route.ProjectRelease version ->
                    if Version.equals currentVersion version then
                        let
                            ( releasePage_, releasePageCmd ) =
                                ProjectReleasePage.update appContext projectRef releasePageMsg releasePage
                        in
                        ( { model | subPage = Release version releasePage_ }
                        , Cmd.map ProjectReleasePageMsg releasePageCmd
                        )

                    else
                        let
                            ( releasePage_, releasePageCmd ) =
                                ProjectReleasePage.init appContext projectRef version
                        in
                        ( { model | subPage = Release version releasePage_ }
                        , Cmd.map ProjectReleasePageMsg releasePageCmd
                        )

                _ ->
                    ( model, Cmd.none )

        ( Releases releasesPage, ProjectReleasesPageMsg releasesPageMsg ) ->
            case route of
                Route.ProjectReleases ->
                    let
                        ( releasesPage_, releasesPageCmd, out ) =
                            ProjectReleasesPage.update appContext projectRef releasesPageMsg releasesPage

                        project =
                            case out of
                                ProjectReleasesPage.None ->
                                    model.project

                                ProjectReleasesPage.PublishedNewRelease r ->
                                    model.project
                                        |> RemoteData.map (\p -> { p | latestVersion = Just r.version })
                    in
                    ( { model | subPage = Releases releasesPage_, project = project }
                    , Cmd.map ProjectReleasesPageMsg releasesPageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Contribution currentContribRef contribPage, ProjectContributionPageMsg contribMsg ) ->
            case route of
                Route.ProjectContribution contribRef contribRoute ->
                    if ContributionRef.equals currentContribRef contribRef then
                        let
                            ( contribPage_, contribPageCmd, contribOut ) =
                                ProjectContributionPage.update appContext
                                    projectRef
                                    contribRef
                                    contribRoute
                                    model.project
                                    contribMsg
                                    contribPage

                            updateNumActiveContributions project =
                                case contribOut of
                                    ProjectContributionPage.ContributionStatusUpdated change ->
                                        case ( change.old, change.new ) of
                                            ( ContributionStatus.InReview, ContributionStatus.Merged ) ->
                                                { project | numActiveContributions = project.numActiveContributions - 1 }

                                            ( ContributionStatus.InReview, ContributionStatus.Archived ) ->
                                                { project | numActiveContributions = project.numActiveContributions - 1 }

                                            ( _, ContributionStatus.InReview ) ->
                                                { project | numActiveContributions = project.numActiveContributions + 1 }

                                            _ ->
                                                project

                                    _ ->
                                        project

                            proj =
                                RemoteData.map updateNumActiveContributions model.project
                        in
                        ( { model | subPage = Contribution contribRef contribPage_, project = proj }
                        , Cmd.map ProjectContributionPageMsg contribPageCmd
                        )

                    else
                        let
                            ( contribPage_, contribPageCmd ) =
                                ProjectContributionPage.init appContext projectRef contribRef contribRoute
                        in
                        ( { model | subPage = Contribution contribRef contribPage_ }
                        , Cmd.map ProjectContributionPageMsg contribPageCmd
                        )

                _ ->
                    ( model, Cmd.none )

        ( Contributions contribsPage, ProjectContributionsPageMsg contribsMsg ) ->
            case route of
                Route.ProjectContributions subRoute ->
                    let
                        ( contribsPage_, contribsPageCmd, out ) =
                            ProjectContributionsPage.update appContext projectRef subRoute model.project contribsMsg contribsPage

                        project =
                            case out of
                                ProjectContributionsPage.NoOut ->
                                    model.project

                                ProjectContributionsPage.AddedContribution ->
                                    model.project
                                        |> RemoteData.map
                                            (\p -> { p | numActiveContributions = p.numActiveContributions + 1 })
                    in
                    ( { model | project = project, subPage = Contributions contribsPage_ }
                    , Cmd.map ProjectContributionsPageMsg contribsPageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Ticket currentTicketRef ticketPage, ProjectTicketPageMsg ticketMsg ) ->
            case route of
                Route.ProjectTicket ticketRef ->
                    if TicketRef.equals currentTicketRef ticketRef then
                        let
                            ( ticketPage_, ticketPageCmd, ticketOut ) =
                                ProjectTicketPage.update appContext
                                    projectRef
                                    ticketRef
                                    ticketMsg
                                    ticketPage

                            updateNumOpenTickets project =
                                case ticketOut of
                                    ProjectTicketPage.TicketStatusUpdated change ->
                                        case ( change.old, change.new ) of
                                            ( TicketStatus.Open, TicketStatus.Closed ) ->
                                                { project | numOpenTickets = project.numOpenTickets - 1 }

                                            ( TicketStatus.Closed, TicketStatus.Open ) ->
                                                { project | numOpenTickets = project.numOpenTickets + 1 }

                                            _ ->
                                                project

                                    _ ->
                                        project

                            proj =
                                RemoteData.map updateNumOpenTickets model.project
                        in
                        ( { model | subPage = Ticket ticketRef ticketPage_, project = proj }
                        , Cmd.map ProjectTicketPageMsg ticketPageCmd
                        )

                    else
                        let
                            ( ticketPage_, ticketPageCmd ) =
                                ProjectTicketPage.init appContext projectRef ticketRef
                        in
                        ( { model | subPage = Ticket ticketRef ticketPage_ }
                        , Cmd.map ProjectTicketPageMsg ticketPageCmd
                        )

                _ ->
                    ( model, Cmd.none )

        ( Tickets ticketsPage, ProjectTicketsPageMsg ticketsMsg ) ->
            case route of
                Route.ProjectTickets ->
                    let
                        ( ticketsPage_, ticketsPageCmd, out ) =
                            ProjectTicketsPage.update appContext projectRef ticketsMsg ticketsPage

                        project =
                            case out of
                                ProjectTicketsPage.NoOut ->
                                    model.project

                                ProjectTicketsPage.AddedTicket ->
                                    model.project
                                        |> RemoteData.map
                                            (\p -> { p | numOpenTickets = p.numOpenTickets + 1 })
                    in
                    ( { model | project = project, subPage = Tickets ticketsPage_ }
                    , Cmd.map ProjectTicketsPageMsg ticketsPageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( Settings settings, ProjectSettingsPageMsg settingsPageMsg ) ->
            case ( model.project, route ) of
                ( Success project, Route.ProjectSettings ) ->
                    let
                        ( settings_, settingsPageCmd, out ) =
                            ProjectSettingsPage.update appContext project settingsPageMsg settings

                        ( project_, modal ) =
                            case out of
                                ProjectSettingsPage.ProjectUpdated p ->
                                    ( p, model.modal )

                                ProjectSettingsPage.ShowDeleteProjectModalRequest ->
                                    ( project, DeleteProjectModal { confirmText = "", deleting = NotAsked } )

                                _ ->
                                    ( project, model.modal )
                    in
                    ( { model
                        | project = Success project_
                        , subPage = Settings settings_
                        , modal = modal
                      }
                    , Cmd.map ProjectSettingsPageMsg settingsPageCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( History _ _ history, ProjectHistoryPageMsg historyPageMsg ) ->
            case ( model.project, route ) of
                ( Success project, Route.ProjectHistory branchRef_ cursor ) ->
                    let
                        ( history_, historyCmd ) =
                            ProjectHistoryPage.update appContext project branchRef_ historyPageMsg history
                    in
                    ( { model | subPage = History branchRef_ cursor history_ }
                    , Cmd.map ProjectHistoryPageMsg historyCmd
                    )

                _ ->
                    ( model, Cmd.none )

        ( _, SwitchBranchMsg sbMsg ) ->
            let
                ( switchBranch, switchBranchCmd, out ) =
                    SwitchBranch.update appContext projectRef sbMsg model.switchBranch

                navCmd =
                    case out of
                        SwitchBranch.SwitchToBranchRequest branchRef ->
                            Route.navigate
                                appContext.navKey
                                (Route.projectBranchRoot
                                    projectRef
                                    branchRef
                                    Perspective.relativeRootPerspective
                                )

                        _ ->
                            Cmd.none
            in
            ( { model | switchBranch = switchBranch }
            , Cmd.batch
                [ Cmd.map SwitchBranchMsg switchBranchCmd
                , navCmd
                ]
            )

        ( _, ShowUseProjectModal ) ->
            ( { model | modal = UseProjectModal }, Cmd.none )

        ( _, CloseModal ) ->
            ( { model | modal = NoModal }, Cmd.none )

        _ ->
            ( model, Cmd.none )


{-| Pass through to CodePage. Used by App when routes change
-}
updateSubPage : AppContext -> ProjectRef -> Model -> ProjectRoute -> ( Model, Cmd Msg )
updateSubPage appContext projectRef model route =
    let
        codeBrowsingContext br =
            { projectRef = projectRef, branchRef = br }

        newCodePage branchRef codeRoute =
            let
                ( codePage, codePageCmd ) =
                    CodePage.init appContext (codeBrowsingContext branchRef) codeRoute
            in
            ( { model | subPage = Code branchRef codePage }
            , Cmd.map CodePageMsg codePageCmd
            )
    in
    case route of
        ProjectOverview ->
            case model.subPage of
                Overview _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( overviewPage, overviewCmd ) =
                            case model.project of
                                Success project ->
                                    ProjectOverviewPage.initWithProject appContext project

                                _ ->
                                    ProjectOverviewPage.init appContext projectRef
                    in
                    ( { model | subPage = Overview overviewPage }, Cmd.map ProjectOverviewPageMsg overviewCmd )

        ProjectBranches branchesRoute cursor ->
            case model.subPage of
                Branches page ->
                    let
                        ( branchesPage, branchesCmd ) =
                            ProjectBranchesPage.updateSubPage
                                appContext
                                projectRef
                                branchesRoute
                                cursor
                                page
                    in
                    ( { model | subPage = Branches branchesPage }, Cmd.map ProjectBranchesPageMsg branchesCmd )

                _ ->
                    let
                        ( branchesPage, branchesCmd ) =
                            ProjectBranchesPage.init
                                appContext
                                projectRef
                                branchesRoute
                                cursor
                    in
                    ( { model | subPage = Branches branchesPage }, Cmd.map ProjectBranchesPageMsg branchesCmd )

        ProjectBranch branchRef codeRoute ->
            case model.subPage of
                Code oldBranchRef codeSubPage ->
                    if BranchRef.equals oldBranchRef branchRef then
                        let
                            ( codePage, codePageCmd ) =
                                CodePage.updateSubPage appContext (codeBrowsingContext branchRef) codeRoute codeSubPage
                        in
                        ( { model | subPage = Code branchRef codePage }
                        , Cmd.map CodePageMsg codePageCmd
                        )

                    else
                        newCodePage branchRef codeRoute

                _ ->
                    newCodePage branchRef codeRoute

        ProjectContribution contribRef contribRoute ->
            let
                newContribPage =
                    let
                        ( contrib_, contribCmd ) =
                            ProjectContributionPage.init appContext projectRef contribRef contribRoute
                    in
                    ( { model | subPage = Contribution contribRef contrib_ }, Cmd.map ProjectContributionPageMsg contribCmd )
            in
            case model.subPage of
                Contribution cRef page ->
                    if ContributionRef.equals cRef contribRef then
                        let
                            ( contrib_, contribCmd ) =
                                ProjectContributionPage.updateSubPage appContext projectRef contribRef contribRoute page
                        in
                        ( { model | subPage = Contribution contribRef contrib_ }, Cmd.map ProjectContributionPageMsg contribCmd )

                    else
                        newContribPage

                _ ->
                    newContribPage

        ProjectContributions subRoute ->
            case model.subPage of
                Contributions page ->
                    let
                        ( contribs_, contribsCmd ) =
                            ProjectContributionsPage.updateSubPage appContext projectRef subRoute page
                    in
                    ( { model | subPage = Contributions contribs_ }, Cmd.map ProjectContributionsPageMsg contribsCmd )

                _ ->
                    let
                        ( contribs_, contribsCmd ) =
                            ProjectContributionsPage.init appContext projectRef subRoute
                    in
                    ( { model | subPage = Contributions contribs_ }, Cmd.map ProjectContributionsPageMsg contribsCmd )

        ProjectTicket ticketRef ->
            case model.subPage of
                Ticket _ _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( ticket_, ticketCmd ) =
                            ProjectTicketPage.init appContext projectRef ticketRef
                    in
                    ( { model | subPage = Ticket ticketRef ticket_ }, Cmd.map ProjectTicketPageMsg ticketCmd )

        ProjectTickets ->
            case model.subPage of
                Tickets _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( tickets_, ticketsCmd ) =
                            ProjectTicketsPage.init appContext projectRef
                    in
                    ( { model | subPage = Tickets tickets_ }, Cmd.map ProjectTicketsPageMsg ticketsCmd )

        ProjectReleases ->
            case model.subPage of
                Releases _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( releasesPage_, releasesPageCmd ) =
                            ProjectReleasesPage.init
                                appContext
                                projectRef
                                (RemoteData.map .latestVersion model.project)
                    in
                    ( { model | subPage = Releases releasesPage_ }, Cmd.map ProjectReleasesPageMsg releasesPageCmd )

        ProjectRelease version ->
            case model.subPage of
                Release _ _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( releasePage_, releasePageCmd ) =
                            ProjectReleasePage.init appContext projectRef version
                    in
                    ( { model | subPage = Release version releasePage_ }, Cmd.map ProjectReleasePageMsg releasePageCmd )

        ProjectHistory branchRef cursor ->
            case model.subPage of
                History _ _ _ ->
                    ( model, Cmd.none )

                _ ->
                    case model.project of
                        Success project ->
                            let
                                ( history, cmd ) =
                                    ProjectHistoryPage.init appContext project branchRef cursor
                            in
                            ( { model | subPage = History branchRef cursor history }, Cmd.map ProjectHistoryPageMsg cmd )

                        _ ->
                            ( model, Cmd.none )

        ProjectSettings ->
            case model.subPage of
                Settings _ ->
                    ( model, Cmd.none )

                _ ->
                    let
                        ( settings, cmd ) =
                            ProjectSettingsPage.init appContext projectRef
                    in
                    ( { model | subPage = Settings settings }, Cmd.map ProjectSettingsPageMsg cmd )



-- EFFECTS


fetchProject : AppContext -> ProjectRef -> Cmd Msg
fetchProject appContext projectRef =
    ShareApi.project projectRef
        |> HttpApi.toRequest Project.decodeDetails (RemoteData.fromResult >> FetchProjectFinished)
        |> HttpApi.perform appContext.api


setProjectFav : AppContext -> ProjectDetails -> Cmd Msg
setProjectFav appContext project =
    let
        isFaved =
            Project.isFavedToBool project.isFaved
    in
    ShareApi.updateProjectFav project.ref isFaved
        |> HttpApi.toRequestWithEmptyResponse (SetProjectFavFinished isFaved)
        |> HttpApi.perform appContext.api


setProjectSubscription : AppContext -> ProjectDetails -> Cmd Msg
setProjectSubscription appContext project =
    let
        isSubscribed =
            Project.isSubscribedToBool project.isSubscribed
    in
    ShareApi.updateProjectSubscription project.ref isSubscribed
        |> HttpApi.toRequestWithEmptyResponse (SetProjectSubscriptionFinished isSubscribed)
        |> HttpApi.perform appContext.api


deleteProject : AppContext -> ProjectRef -> Cmd Msg
deleteProject appContext projectRef =
    ShareApi.deleteProject projectRef
        |> HttpApi.toRequestWithEmptyResponse DeleteProjectFinished
        |> HttpApi.perform appContext.api



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.subPage of
        Overview _ ->
            Sub.map ProjectOverviewPageMsg ProjectOverviewPage.subscriptions

        Code _ ucp ->
            Sub.map CodePageMsg (CodePage.subscriptions ucp)

        _ ->
            Sub.none



-- EFFECTS


navigateToCode : AppContext -> ProjectRef -> BranchRef -> CodeRoute -> Cmd Msg
navigateToCode appContext projectRef branchRef codeRoute =
    Route.navigate appContext.navKey (Route.projectBranch projectRef branchRef codeRoute)



-- VIEW


viewDeleteProjectModal : ProjectRef -> DeleteProject -> Html Msg
viewDeleteProjectModal projectRef { confirmText, deleting } =
    let
        projectRef_ =
            ProjectRef.toString projectRef

        ( statusBanner, overlay ) =
            case deleting of
                NotAsked ->
                    ( UI.nothing, UI.nothing )

                Loading ->
                    ( StatusBanner.working "Deleting..", div [ class "delete-project-modal_overlay-deleting" ] [] )

                Success _ ->
                    ( UI.nothing
                    , div
                        [ class "delete-project-modal_overlay-success"
                        ]
                        [ StatusIndicator.good |> StatusIndicator.large |> StatusIndicator.view
                        , div []
                            [ strong [] [ text projectRef_ ]
                            , br [] []
                            , text " successfully deleted"
                            ]
                        ]
                    )

                Failure _ ->
                    ( StatusBanner.bad "Delete project failed", UI.nothing )

        content =
            div [ class "delete-project-modal_content" ]
                [ p []
                    [ text "You're about to permanently delete "
                    , strong [] [ text projectRef_ ]
                    , text "."
                    ]
                , StatusBanner.bad "Take care—project deletions can't be undone!"
                , Divider.divider
                    |> Divider.small
                    |> Divider.withoutMargin
                    |> Divider.view
                , form [ class "description-form" ]
                    [ TextField.field UpdateDeleteProjectModalConfirmText "Type \"delete\" to confirm." confirmText
                        |> TextField.withAutofocus
                        |> TextField.view
                    ]
                , footer
                    [ class "delete-project-modal_actions" ]
                    [ statusBanner
                    , Button.button CloseModal "Cancel"
                        |> Button.subdued
                        |> Button.medium
                        |> Button.view
                    , Button.button YesDeleteProject "Yes, delete project"
                        |> Button.critical
                        |> Button.medium
                        |> Button.view
                    ]
                , overlay
                ]
    in
    Modal.modal "delete-project-modal" CloseModal (Modal.Content content)
        |> Modal.withAccept YesDeleteProject
        |> Modal.withHeader "Permanently Delete Project?"
        |> Modal.view


viewUseProjectModal : ProjectDetails -> Maybe BranchRef -> Html Msg
viewUseProjectModal project branchRef =
    let
        projectRef_ =
            ProjectRef.toString project.ref

        libVersion v =
            UserHandle.toUnprefixedString (ProjectRef.handle project.ref)
                ++ "_"
                ++ ProjectSlug.toNamespaceString (ProjectRef.slug project.ref)
                ++ "_"
                ++ Version.toNamespaceString v

        installCommand_ br =
            let
                sameRelease =
                    case ( project.latestVersion, BranchRef.version br ) of
                        ( Just lv, Just bv ) ->
                            lv == bv

                        _ ->
                            False

                cmd =
                    if sameRelease then
                        UcmCommand.Install project.ref Nothing

                    else
                        UcmCommand.Install project.ref (Just br)
            in
            UcmCommand.toString cmd

        installHint_ source =
            [ text "UCM will install the ", strong [] [ text source ], text " into the lib namespace." ]

        upgradeHint_ v =
            div [ class "upgrade-hint" ]
                [ div [ class "upgrade-icon" ] [ Icon.view Icon.arrowUp ]
                , div [ class "upgrade-hint_content" ]
                    [ div [] [ text "Upgrading from a previous version? Install using the above and then run:" ]
                    , div [ class "monospace" ] [ text ("myProject/main> upgrade <old_version> " ++ libVersion v) ]
                    ]
                ]

        { activeBranchRef, modalTitle, installCommand, installHint, upgradeHint } =
            case ( branchRef, project.latestVersion, project.defaultBranch ) of
                ( Just b, _, _ ) ->
                    case b of
                        BranchRef.ReleaseBranchRef v ->
                            { activeBranchRef = b
                            , modalTitle = "/" ++ BranchRef.toString b
                            , installCommand = installCommand_ b
                            , installHint = installHint_ (BranchRef.toString b ++ " release")
                            , upgradeHint = Just (upgradeHint_ v)
                            }

                        _ ->
                            { activeBranchRef = b
                            , modalTitle = "/" ++ BranchRef.toString b
                            , installCommand = installCommand_ b
                            , installHint = installHint_ (BranchRef.toString b ++ " branch")
                            , upgradeHint = Nothing
                            }

                ( Nothing, Just v, _ ) ->
                    { activeBranchRef = BranchRef.ReleaseBranchRef v
                    , modalTitle = ""
                    , installCommand = installCommand_ (BranchRef.releaseBranchRef v)
                    , installHint = installHint_ "latest release"
                    , upgradeHint = Just (upgradeHint_ v)
                    }

                ( Nothing, Nothing, Just b ) ->
                    { activeBranchRef = b
                    , modalTitle = "/" ++ BranchRef.toString b
                    , installCommand = installCommand_ b
                    , installHint = installHint_ (BranchRef.toString b ++ " branch")
                    , upgradeHint = Nothing
                    }

                _ ->
                    { activeBranchRef = BranchRef.main_
                    , modalTitle = "/main"
                    , installCommand = installCommand_ BranchRef.main_
                    , installHint = installHint_ "main branch"
                    , upgradeHint = Nothing
                    }

        {- for contribution, people typically want to clone main or a selected branch, not the latest release -}
        ( cloneBranch, cloneCommand ) =
            case ( branchRef, project.defaultBranch ) of
                ( Just b, Just db ) ->
                    case b of
                        BranchRef.ReleaseBranchRef _ ->
                            ( db, "clone " ++ projectRef_ )

                        _ ->
                            ( b, "clone " ++ projectRef_ ++ "/" ++ BranchRef.toString b )

                ( Just b, Nothing ) ->
                    case b of
                        BranchRef.ReleaseBranchRef _ ->
                            ( BranchRef.main_, "clone " ++ projectRef_ )

                        _ ->
                            ( b, "clone " ++ projectRef_ ++ "/" ++ BranchRef.toString b )

                ( Nothing, Just b ) ->
                    ( b, "clone " ++ projectRef_ )

                _ ->
                    ( BranchRef.main_, "clone " ++ projectRef_ ++ "/" ++ BranchRef.toString BranchRef.main_ )

        notAReleaseNote =
            case activeBranchRef of
                BranchRef.ReleaseBranchRef _ ->
                    UI.nothing

                _ ->
                    StatusBanner.info_
                        (span []
                            [ text "Note that "
                            , strong [] [ text (BranchRef.toString activeBranchRef) ]
                            , text " is a branch, not the latest release."
                            ]
                        )

        content =
            Modal.Content
                (div [ class "use-project-modal_content" ]
                    [ notAReleaseNote
                    , div [ class "instruction" ]
                        [ h3 [] [ text "As a dependency" ]
                        , p []
                            [ text "From within your project in UCM, run the "
                            , strong [] [ text "lib.install" ]
                            , text " command:"
                            ]
                        , CopyField.copyField (\_ -> NoOp) installCommand |> CopyField.withPrefix "myProject/main>" |> CopyField.view
                        , div [ class "hint" ] installHint
                        , Maybe.withDefault UI.nothing upgradeHint
                        ]
                    , Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view
                    , div [ class "instruction" ]
                        [ h3 [] [ text "To contribute" ]
                        , p [] [ text "Run the ", strong [] [ text "clone" ], text " command within UCM:" ]
                        , CopyField.copyField (\_ -> NoOp) cloneCommand |> CopyField.withPrefix ".>" |> CopyField.view
                        , div [ class "hint" ]
                            [ text "UCM will clone the "
                            , strong [] [ text (BranchRef.toString cloneBranch) ]
                            , text " branch of this project, and switch you to it."
                            ]
                        ]
                    , div [ class "action" ] [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" |> Button.emphasized |> Button.view ]
                    ]
                )
    in
    Modal.modal "use-project-modal" CloseModal content
        |> Modal.withHeader ("Use " ++ projectRef_ ++ modalTitle)
        |> Modal.view


viewErrorPage : AppContext -> SubPage -> ProjectRef -> Error -> AppDocument msg
viewErrorPage appContext subPage projectRef error =
    let
        pageHeader =
            ProjectPageHeader.error projectRef

        projectRef_ =
            ProjectRef.toString projectRef

        errorCard =
            case error of
                BadStatus 404 ->
                    let
                        signInSuggest =
                            case appContext.session of
                                Session.Anonymous ->
                                    div [ class "project-page_login-tip" ] [ StatusBanner.info "Tip: if you're looking for a private project, try signing in." ]

                                _ ->
                                    UI.nothing
                    in
                    div []
                        [ ErrorCard.errorCard
                            ("Couldn't find " ++ projectRef_)
                            ("We looked everywhere, but unfortunately '" ++ projectRef_ ++ "' was nowhere to be found.")
                            |> ErrorCard.toCard
                            |> Card.asContainedWithFade
                            |> Card.view
                        , signInSuggest
                        ]

                _ ->
                    ErrorCard.errorCard
                        ("Couldn't load " ++ projectRef_)
                        "Something unexpected happened on our end when loading the Project and we can't display it."
                        |> ErrorCard.toCard
                        |> Card.asContainedWithFade
                        |> Card.view

        page =
            case subPage of
                Overview _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Branches _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Code _ _ ->
                    PageLayout.sidebarLeftContentLayout
                        appContext.operatingSystem
                        (Sidebar.empty "main-sidebar")
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Contribution _ _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Contributions _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Ticket _ _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Tickets _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Release _ _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Releases _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                History _ _ _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground

                Settings _ ->
                    PageLayout.centeredNarrowLayout
                        (PageContent.oneColumn [ errorCard ])
                        PageFooter.pageFooter
                        |> PageLayout.withSubduedBackground
    in
    { pageId = "project-page project-page-error"
    , title = "Error loading " ++ ProjectRef.toString projectRef
    , appHeader = AppHeader.appHeader
    , pageHeader = Just pageHeader
    , page = PageLayout.view page
    , modal = Nothing
    }


viewLoadingPage : AppContext -> SubPage -> ProjectRef -> AppDocument Msg
viewLoadingPage appContext subPage projectRef =
    let
        ( page, pageId ) =
            case subPage of
                Overview _ ->
                    ( ProjectOverviewPage.viewLoadingPage projectRef, "project-overview-page" )

                Branches _ ->
                    ( ProjectBranchesPage.viewLoadingPage projectRef, "project-branches-page" )

                Code _ _ ->
                    ( PageLayout.sidebarLeftContentLayout
                        appContext.operatingSystem
                        (Sidebar.empty "main-sidebar")
                        (PageContent.oneColumn [ text "" ])
                        PageFooter.pageFooter
                    , "code-page"
                    )

                Contribution _ _ ->
                    ( PageLayout.map
                        ProjectContributionPageMsg
                        ProjectContributionPage.viewLoadingPage
                    , "project-contribution-page"
                    )

                Contributions _ ->
                    ( PageLayout.map
                        ProjectContributionsPageMsg
                        (ProjectContributionsPage.viewLoadingPage
                            (ProjectContributionsPage.InReview Loading)
                            projectRef
                        )
                    , "project-contributions-page"
                    )

                Ticket _ _ ->
                    ( PageLayout.map
                        ProjectTicketPageMsg
                        ProjectTicketPage.viewLoadingPage
                    , "project-ticket-page"
                    )

                Tickets _ ->
                    ( PageLayout.map
                        ProjectTicketsPageMsg
                        ProjectTicketsPage.viewLoadingPage
                    , "project-tickets-page"
                    )

                Release version _ ->
                    ( PageLayout.map
                        ProjectReleasePageMsg
                        (ProjectReleasePage.viewLoadingPage version)
                    , "project-release-page"
                    )

                Releases _ ->
                    ( PageLayout.map
                        ProjectReleasesPageMsg
                        (ProjectReleasesPage.viewLoadingPage projectRef)
                    , "project-releases-page"
                    )

                History _ _ _ ->
                    ( ProjectHistoryPage.viewLoadingPage, "project-history-page" )

                Settings _ ->
                    ( ProjectSettingsPage.viewLoadingPage, "project-settings-page" )
    in
    { pageId = "project-page project-page-loading " ++ pageId
    , title = "Loading " ++ ProjectRef.toString projectRef
    , appHeader = AppHeader.appHeader
    , pageHeader = Just (ProjectPageHeader.loading projectRef)
    , page = PageLayout.view page
    , modal = Nothing
    }


viewProjectEmptyState : ProjectDetails -> Maybe (Html Msg) -> AppDocument Msg
viewProjectEmptyState project modal =
    let
        content =
            if Project.canManage project then
                [ EmptyState.iconCloud (EmptyState.IconCenterPiece Icon.branch)
                    |> EmptyState.withContent
                        [ h1 [] [ text "No default branch" ]
                        , div [ class "empty-state-content" ]
                            [ p []
                                [ text "This happens when you create a project, but haven't pushed a branch called "
                                , UI.inlineCode [] (text "/main")
                                , text " yet. "
                                , br [] []
                                , text "Share requires a "
                                , UI.inlineCode [] (text "/main")
                                , text " branch to exist for full project functionality."
                                ]
                            , p
                                [ class "delete-project" ]
                                [ text "Alternatively, you can "
                                , Button.iconThenLabel ShowDeleteProjectModal Icon.trash "Delete this project"
                                    |> Button.small
                                    |> Button.subdued
                                    |> Button.view
                                , text " and start over."
                                ]
                            ]
                        ]
                    |> EmptyStateCard.view
                ]

            else
                [ text "Nothing to see here yet." ]

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn content)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
    in
    { pageId = "project-page project-page-empty"
    , title = ProjectRef.toString project.ref
    , appHeader = AppHeader.appHeader
    , pageHeader = Just (ProjectPageHeader.disabled project.ref)
    , page = PageLayout.view page
    , modal = modal
    }


view : AppContext -> ProjectRef -> Model -> AppDocument Msg
view appContext projectRef model =
    let
        pageHeader activeNavItem branchRef project =
            ProjectPageHeader.projectPageHeader
                appContext.session
                { toggleFavMsg = ToggleProjectFav
                , toggleSubscriptionMsg = ToggleProjectSubscription
                , useProjectButtonClickMsg = ShowUseProjectModal
                , mobileNavToggleMsg = ToggleMobileNav
                , mobileNavIsOpen = model.mobileNavIsOpen
                , activeNavItem = activeNavItem
                , switchBranch =
                    AnchoredOverlay.map
                        SwitchBranchMsg
                        (SwitchBranch.toAnchoredOverlay projectRef branchRef True model.switchBranch)
                , contextClick = Click.onClick (ChangeRouteTo (Route.projectOverview projectRef))
                }
                project

        title =
            ProjectRef.toString projectRef

        appHeader =
            AppHeader.appHeader

        modal pageModal =
            case ( model.project, model.modal ) of
                ( Success p, UseProjectModal ) ->
                    case model.subPage of
                        Code br _ ->
                            Just (viewUseProjectModal p (Just br))

                        _ ->
                            Just (viewUseProjectModal p Nothing)

                ( _, DeleteProjectModal deleteProject_ ) ->
                    Just (viewDeleteProjectModal projectRef deleteProject_)

                _ ->
                    pageModal
    in
    case model.project of
        NotAsked ->
            viewLoadingPage appContext model.subPage projectRef

        Loading ->
            viewLoadingPage appContext model.subPage projectRef

        Failure error ->
            viewErrorPage appContext model.subPage projectRef error

        Success project ->
            case project.defaultBranch of
                Nothing ->
                    viewProjectEmptyState project (modal Nothing)

                Just _ ->
                    case model.subPage of
                        Overview overviewPage ->
                            let
                                ( overviewPage_, modal_ ) =
                                    ProjectOverviewPage.view
                                        appContext.session
                                        projectRef
                                        project
                                        overviewPage
                            in
                            { pageId = "project-page project-overview-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader ProjectPageHeader.Overview
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectOverviewPageMsg overviewPage_)
                            , modal = modal (Maybe.map (Html.map ProjectOverviewPageMsg) modal_)
                            }

                        Branches branchesPage ->
                            let
                                ( branchesPage_, modal_ ) =
                                    ProjectBranchesPage.view
                                        appContext
                                        project
                                        branchesPage
                            in
                            { pageId = "project-page project-branches-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader ProjectPageHeader.NoActiveNavItem
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectBranchesPageMsg branchesPage_)
                            , modal = modal (Maybe.map (Html.map ProjectBranchesPageMsg) modal_)
                            }

                        Code branchRef codePage ->
                            let
                                ( codePage_, modal_ ) =
                                    CodePage.view appContext CodePageMsg codePage
                            in
                            { pageId = "project-page code-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader ProjectPageHeader.DocsAndCode
                                        branchRef
                                        project
                                    )
                            , page = PageLayout.view codePage_
                            , modal = modal modal_
                            }

                        Release version release ->
                            let
                                ( release_, modal_ ) =
                                    ProjectReleasePage.view
                                        appContext
                                        project.ref
                                        version
                                        release
                            in
                            { pageId = "project-page project-release-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Releases
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectReleasePageMsg release_)
                            , modal = modal (Maybe.map (Html.map ProjectReleasePageMsg) modal_)
                            }

                        Releases releases ->
                            let
                                ( releases_, modal_ ) =
                                    ProjectReleasesPage.view appContext project releases
                            in
                            { pageId = "project-page project-releases-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Releases
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectReleasesPageMsg releases_)
                            , modal = modal (Maybe.map (Html.map ProjectReleasesPageMsg) modal_)
                            }

                        Contribution cRef contribution ->
                            let
                                ( contribution_, modal_ ) =
                                    ProjectContributionPage.view
                                        appContext
                                        project
                                        cRef
                                        contribution
                            in
                            { pageId = "project-page project-contribution-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Contributions
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectContributionPageMsg contribution_)
                            , modal = modal (Maybe.map (Html.map ProjectContributionPageMsg) modal_)
                            }

                        Contributions contributions ->
                            let
                                ( contributions_, modal_ ) =
                                    ProjectContributionsPage.view
                                        appContext
                                        project
                                        contributions
                            in
                            { pageId = "project-page project-contributions-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Contributions
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectContributionsPageMsg contributions_)
                            , modal = modal (Maybe.map (Html.map ProjectContributionsPageMsg) modal_)
                            }

                        Ticket tRef ticket ->
                            let
                                ( ticket_, modal_ ) =
                                    ProjectTicketPage.view
                                        appContext
                                        project
                                        tRef
                                        ticket
                            in
                            { pageId = "project-page project-ticket-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Tickets
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectTicketPageMsg ticket_)
                            , modal = modal (Maybe.map (Html.map ProjectTicketPageMsg) modal_)
                            }

                        Tickets tickets ->
                            let
                                ( tickets_, modal_ ) =
                                    ProjectTicketsPage.view
                                        appContext
                                        project
                                        tickets
                            in
                            { pageId = "project-page project-tickets-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Tickets
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectTicketsPageMsg tickets_)
                            , modal = modal (Maybe.map (Html.map ProjectTicketsPageMsg) modal_)
                            }

                        History branchRef cursor history ->
                            let
                                ( history_, modal_ ) =
                                    ProjectHistoryPage.view appContext project branchRef cursor history
                            in
                            { pageId = "project-page project-history-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.NoActiveNavItem
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectHistoryPageMsg history_)
                            , modal = modal (Maybe.map (Html.map ProjectHistoryPageMsg) modal_)
                            }

                        Settings settings ->
                            let
                                ( settings_, modal_ ) =
                                    ProjectSettingsPage.view appContext.session project settings
                            in
                            { pageId = "project-page project-settings-page"
                            , title = title
                            , appHeader = appHeader
                            , pageHeader =
                                Just
                                    (pageHeader
                                        ProjectPageHeader.Settings
                                        (Project.defaultBrowsingBranch project)
                                        project
                                    )
                            , page = PageLayout.view (PageLayout.map ProjectSettingsPageMsg settings_)
                            , modal = modal (Maybe.map (Html.map ProjectSettingsPageMsg) modal_)
                            }
