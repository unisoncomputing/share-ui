module UnisonShare.Page.ProjectOverviewPage exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Config exposing (Config)
import Code.Definition.Readme as Readme exposing (Readme)
import Code.Finder as Finder
import Code.Finder.SearchOptions as SearchOptions
import Code.FullyQualifiedName as FQN
import Code.Namespace.NamespaceRef as NamespaceRef
import Code.Perspective as Perspective
import Code.ReadmeCard as ReadmeCard
import Html exposing (Html, div, footer, form, p, span, strong, text)
import Html.Attributes exposing (class)
import Http exposing (Error)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (requiredAt)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util exposing (unicodeStringLength)
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.CopyField as CopyField
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key exposing (Key(..))
import UI.KeyboardShortcut.KeyboardEvent as KeyboardEvent exposing (KeyboardEvent)
import UI.KpiTag as KpiTag
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.StatusIndicator as StatusIndicator
import UI.Steps as Steps
import UI.Tag as Tag
import UI.Tooltip as Tooltip
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.CodeBrowsingContext as CodeBrowsingContext exposing (CodeBrowsingContext(..))
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import UnisonShare.Session as Session exposing (Session)



-- MODEL


{-| these are formfield values, so they are less strict than their equivalent
Project fields; `tags`, for instance, is a string separated by space as opposed
to `Set String`.
-}
type alias EditDescriptionFormFields =
    { summary : String, tags : String }


type EditDescriptionForm
    = Editing EditDescriptionFormFields
    | Saving EditDescriptionFormFields
    | SaveSuccessful
    | SaveFailed EditDescriptionFormFields Error


type ProjectOverviewModal
    = NoModal
    | FinderModal Finder.Model
    | ReadmeInstructionsModal
    | EditDescriptionModal EditDescriptionForm


type alias Model =
    { readme : WebData ( Maybe Readme, ReadmeCard.Model )
    , modal : ProjectOverviewModal
    , keyboardShortcut : KeyboardShortcut.Model
    , dependencies : WebData (List ProjectDependency)
    }


init : AppContext -> ProjectRef -> ( Model, Cmd Msg )
init appContext projectRef =
    ( { readme = Loading
      , modal = NoModal
      , keyboardShortcut = KeyboardShortcut.init appContext.operatingSystem
      , dependencies = NotAsked
      }
    , fetchReadme appContext projectRef
    )



-- UPDATE


type Msg
    = NoOp
    | FetchReadmeFinished (WebData (Maybe Readme))
    | RetryFetchReadme
    | FetchDependenciesFinished (WebData (List ProjectDependency))
    | ShowReadmeInstructionsModal
    | ShowEditDescriptionModal
    | ShowFinderModal
    | UpdateSummaryField String
    | UpdateTagsField String
    | SaveDescription
    | SaveDescriptionFinished (HttpResult ())
    | CloseModal
    | ToggleProjectFav
    | ShowUseProjectModal
    | Keydown KeyboardEvent
    | KeyboardShortcutMsg KeyboardShortcut.Msg
    | ReadmeCardMsg ReadmeCard.Msg
    | FinderMsg Finder.Msg


type OutMsg
    = None
      -- The InstallModal is tracked by the ProjectPage module (since it can be
      -- opened from multiple places)
    | RequestToShowUseProjectModal
    | RequestToToggleProjectFav
    | ProjectDescriptionUpdated { summary : Maybe String, tags : Set String }


update : AppContext -> ProjectRef -> WebData ProjectDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef project msg model =
    let
        config =
            case project of
                Success p ->
                    AppContext.toCodeConfig appContext
                        (CodeBrowsingContext.project projectRef (Project.defaultBrowsingBranch p))
                        Perspective.relativeRootPerspective

                _ ->
                    AppContext.toCodeConfig appContext
                        (CodeBrowsingContext.project projectRef BranchRef.main_)
                        Perspective.relativeRootPerspective
    in
    case msg of
        FetchReadmeFinished readme ->
            let
                readme_ =
                    RemoteData.map (\r -> ( r, ReadmeCard.init )) readme
            in
            ( { model | readme = readme_ }, Cmd.none, None )

        RetryFetchReadme ->
            ( { model | readme = Loading }, fetchReadme appContext projectRef, None )

        FetchDependenciesFinished deps ->
            ( { model | dependencies = deps }, Cmd.none, None )

        ShowReadmeInstructionsModal ->
            ( { model | modal = ReadmeInstructionsModal }, Cmd.none, None )

        ShowEditDescriptionModal ->
            case project of
                Success p ->
                    let
                        form =
                            Editing
                                { summary = Maybe.withDefault "" p.summary
                                , tags = p.tags |> Set.toList |> String.join " "
                                }
                    in
                    ( { model | modal = EditDescriptionModal form }, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        ShowFinderModal ->
            let
                ( fm, fCmd ) =
                    Finder.init config (SearchOptions.init config.perspective Nothing)
            in
            ( { model | modal = FinderModal fm }, Cmd.map FinderMsg fCmd, None )

        UpdateSummaryField s ->
            case model.modal of
                EditDescriptionModal (Editing f) ->
                    let
                        form =
                            if unicodeStringLength s <= 100 then
                                Editing { f | summary = s }

                            else
                                Editing f
                    in
                    ( { model | modal = EditDescriptionModal form }, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        UpdateTagsField t ->
            case model.modal of
                EditDescriptionModal (Editing f) ->
                    let
                        form =
                            Editing { f | tags = t }
                    in
                    ( { model | modal = EditDescriptionModal form }, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        SaveDescription ->
            case model.modal of
                EditDescriptionModal (Editing f) ->
                    let
                        modal =
                            EditDescriptionModal (Saving f)
                    in
                    ( { model | modal = modal }
                    , updateProjectDescription appContext projectRef f
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        SaveDescriptionFinished (Ok _) ->
            case model.modal of
                EditDescriptionModal (Saving f) ->
                    ( { model | modal = EditDescriptionModal SaveSuccessful }
                    , Util.delayMsg 1500 CloseModal
                    , ProjectDescriptionUpdated (editDescriptionFieldsToDescriptionUpdate f)
                    )

                _ ->
                    ( model, Cmd.none, None )

        SaveDescriptionFinished (Err e) ->
            case model.modal of
                EditDescriptionModal (Saving f) ->
                    ( { model | modal = EditDescriptionModal (SaveFailed f e) }
                    , Cmd.none
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, None )

        ToggleProjectFav ->
            ( model, Cmd.none, RequestToToggleProjectFav )

        ShowUseProjectModal ->
            ( model, Cmd.none, RequestToShowUseProjectModal )

        Keydown event ->
            keydown appContext model config event

        KeyboardShortcutMsg kMsg ->
            let
                ( keyboardShortcut, kCmd ) =
                    KeyboardShortcut.update kMsg model.keyboardShortcut
            in
            ( { model | keyboardShortcut = keyboardShortcut }, Cmd.map KeyboardShortcutMsg kCmd, None )

        ReadmeCardMsg rMsg ->
            case model.readme of
                Success ( r, card ) ->
                    let
                        ( readmeCard, rCmd, out ) =
                            ReadmeCard.update config rMsg card

                        browsingBranch =
                            case project of
                                Success p ->
                                    Project.defaultBrowsingBranch p

                                _ ->
                                    BranchRef.main_

                        navCmd =
                            case out of
                                ReadmeCard.OpenDefinition ref ->
                                    Route.navigate
                                        appContext.navKey
                                        (Route.projectBranchDefinition projectRef browsingBranch Perspective.relativeRootPerspective ref)

                                _ ->
                                    Cmd.none
                    in
                    ( { model | readme = Success ( r, readmeCard ) }, Cmd.batch [ Cmd.map ReadmeCardMsg rCmd, navCmd ], None )

                _ ->
                    ( model, Cmd.none, None )

        FinderMsg finderMsg ->
            case model.modal of
                FinderModal fm ->
                    let
                        ( fm_, fCmd, outMsg ) =
                            Finder.update config finderMsg fm
                    in
                    case outMsg of
                        Finder.Remain ->
                            ( { model | modal = FinderModal fm_ }, Cmd.map FinderMsg fCmd, None )

                        Finder.Exit ->
                            ( { model | modal = NoModal }, Cmd.map FinderMsg fCmd, None )

                        Finder.OpenDefinition r ->
                            let
                                branchRef =
                                    case project of
                                        Success p ->
                                            Project.defaultBrowsingBranch p

                                        _ ->
                                            BranchRef.main_
                            in
                            ( { model | modal = NoModal }
                            , Cmd.batch
                                [ Cmd.map FinderMsg fCmd
                                , Route.navigate appContext.navKey
                                    (Route.projectBranch projectRef
                                        branchRef
                                        (Route.definition config.perspective r)
                                    )
                                ]
                            , None
                            )

                _ ->
                    ( model, Cmd.none, None )

        NoOp ->
            ( model, Cmd.none, None )



-- HELPERS


keydown : AppContext -> Model -> Config -> KeyboardEvent -> ( Model, Cmd Msg, OutMsg )
keydown appContext model config keyboardEvent =
    let
        shortcut =
            KeyboardShortcut.fromKeyboardEvent model.keyboardShortcut keyboardEvent

        noOp =
            ( model, Cmd.none, None )
    in
    if Finder.isShowFinderKeyboardShortcut appContext.operatingSystem shortcut then
        let
            ( finder, cmd ) =
                Finder.init config
                    (SearchOptions.init config.perspective Nothing)
        in
        ( { model | modal = FinderModal finder }, Cmd.map FinderMsg cmd, None )

    else
        noOp


editDescriptionFieldsToDescriptionUpdate : EditDescriptionFormFields -> { summary : Maybe String, tags : Set String }
editDescriptionFieldsToDescriptionUpdate f =
    let
        summary =
            if String.isEmpty f.summary then
                Nothing

            else
                Just f.summary

        tags =
            f.tags
                |> String.toLower
                |> String.split " "
                |> List.filter (String.isEmpty >> not)
                |> Set.fromList
    in
    { summary = summary, tags = tags }



-- EFFECTS


fetchReadme : AppContext -> ProjectRef -> Cmd Msg
fetchReadme appContext projectRef =
    ShareApi.projectReadme projectRef
        |> HttpApi.toRequest (Decode.field "readMe" (Decode.nullable Readme.decode))
            (RemoteData.fromResult >> FetchReadmeFinished)
        |> HttpApi.perform appContext.api


fetchDependenciesAndUpdate : AppContext -> ProjectDetails -> Model -> ( Model, Cmd Msg )
fetchDependenciesAndUpdate appContext project model =
    let
        branchRef =
            case ( project.latestVersion, project.defaultBranch ) of
                ( Just v, _ ) ->
                    BranchRef.ReleaseBranchRef v

                ( Nothing, Just br ) ->
                    br

                _ ->
                    BranchRef.main_
    in
    ( { model | dependencies = Loading }
    , fetchDependencies appContext project.ref branchRef
    )


fetchDependencies : AppContext -> ProjectRef -> BranchRef -> Cmd Msg
fetchDependencies appContext projectRef branchRef =
    let
        browsingContext =
            ProjectBranch projectRef branchRef

        perspective =
            Perspective.relativeRootPerspective

        namespace =
            NamespaceRef.NameRef (FQN.fromString "lib")

        decodeDependency =
            Decode.succeed ProjectDependency.fromString
                |> requiredAt [ "contents", "namespaceName" ] Decode.string
    in
    ShareApi.browseCodebase browsingContext perspective (Just namespace)
        |> HttpApi.toRequest (Decode.field "namespaceListingChildren" (Decode.list (when (Decode.field "tag" Decode.string) ((==) "Subnamespace") decodeDependency)))
            (RemoteData.fromResult >> FetchDependenciesFinished)
        |> HttpApi.perform appContext.api


updateProjectDescription : AppContext -> ProjectRef -> EditDescriptionFormFields -> Cmd Msg
updateProjectDescription appContext projectRef fields =
    ShareApi.updateProject projectRef
        (ShareApi.ProjectDescriptionUpdate (editDescriptionFieldsToDescriptionUpdate fields))
        |> HttpApi.toRequestWithEmptyResponse SaveDescriptionFinished
        |> HttpApi.perform appContext.api



-- SUBSCRIPTIONS


subscriptions : Sub Msg
subscriptions =
    KeyboardEvent.subscribe KeyboardEvent.Keydown Keydown



-- VIEW


viewLoadingPage : ProjectRef -> PageLayout msg
viewLoadingPage projectRef =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        listing =
            ProjectListing.projectListing
                { ref = projectRef
                , visibility = Project.Public
                }
                |> ProjectListing.huge
                |> ProjectListing.subdued
                |> ProjectListing.view

        content =
            PageContent.oneColumn
                [ ReadmeCard.viewLoading ]
                |> PageContent.withPageTitle (PageTitle.custom [ listing ] |> PageTitle.withRightSide [ shape Placeholder.Large ])
                |> PageContent.withLeftAside
                    [ div [ class "project-overview-page_sidebar_loading" ]
                        [ shape Placeholder.Small
                        , shape Placeholder.Tiny
                        , shape Placeholder.Medium
                        ]
                    ]
    in
    PageLayout.centeredLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewReadmeInstructionsModal : ProjectRef -> Html Msg
viewReadmeInstructionsModal projectRef =
    let
        step1 =
            Steps.step "Write the README"
                [ p [] [ text "Define a Unison term named README and add it to your codebase in the root of your Project." ]
                ]

        pushCommand =
            "push " ++ ProjectRef.toString projectRef

        pullCommand =
            "pull " ++ ProjectRef.toString projectRef

        step2 =
            Steps.step "Push the README to Unison Share"
                [ p []
                    [ text "Push your new README to your Project on Unison Share." ]
                , CopyField.copyField (\_ -> NoOp) pushCommand |> CopyField.withPrefix ".>" |> CopyField.view
                , p [ class "pull-hint" ]
                    [ Icon.view Icon.bulb
                    , span []
                        [ text "If you have code on Unison Share already, pulling with "
                        , span [ class "inline-code" ] [ text pullCommand ]
                        , text " might be needed first."
                        ]
                    ]
                ]

        steps =
            Steps.steps step1 [ step2 ]

        content =
            Modal.Content
                (div
                    []
                    [ p [] [ text "A Project README is an excellent way to provide in-depth status, details, and documentation for your Project." ]
                    , UI.divider
                    , Steps.view steps
                    , footer [ class "modal-actions" ]
                        [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!"
                            |> Button.emphasized
                            |> Button.medium
                            |> Button.view
                        ]
                    ]
                )
    in
    Modal.modal "project-readme-instructions-modal" CloseModal content
        |> Modal.withHeader "Customize your Project with a README"
        |> Modal.view


viewEditDescriptionModal : EditDescriptionForm -> Html Msg
viewEditDescriptionModal descriptionForm =
    let
        form_ f =
            form [ class "description-form" ]
                [ p [] [ text "Describe your project with a brief summary and topic tags." ]
                , TextField.field UpdateSummaryField "Summary" f.summary
                    |> TextField.withHelpText (String.fromInt (unicodeStringLength f.summary) ++ "/100 characters.")
                    |> TextField.withRows 2
                    |> TextField.withMaxlength 100
                    |> TextField.withAutofocus
                    |> TextField.view

                {--, TextField.field UpdateTagsField "Topic tags" f.tags
                    |> TextField.withHelpText "Separate with spaces."
                    |> TextField.view
                    --}
                ]

        buttons_ =
            { cancel =
                Button.button CloseModal "Cancel"
                    |> Button.subdued
                    |> Button.medium
            , save =
                Button.button SaveDescription "Save"
                    |> Button.emphasized
                    |> Button.medium
            }

        disabledButtons =
            { cancel = Button.disabled buttons_.cancel, save = Button.disabled buttons_.save }

        content_ buttons stateClass message form__ =
            div
                [ class "edit-project-description-modal_content", class stateClass ]
                [ form__
                , footer [ class "actions" ]
                    [ message
                    , div [ class "buttons" ]
                        [ buttons.cancel |> Button.view
                        , buttons.save |> Button.view
                        ]
                    ]
                ]

        content =
            case descriptionForm of
                Editing f ->
                    content_ buttons_ "editing" UI.nothing (form_ f)

                Saving f ->
                    content_ disabledButtons "saving" (StatusBanner.working "Saving..") (form_ f)

                SaveSuccessful ->
                    div [ class "save-success" ]
                        [ StatusIndicator.good
                            |> StatusIndicator.large
                            |> StatusIndicator.view
                        ]

                SaveFailed f _ ->
                    content_ buttons_ "save-failed" (StatusBanner.bad "Save failed, try again") (form_ f)
    in
    Modal.modal "edit-project-description-modal" CloseModal (Modal.Content content)
        |> Modal.withHeader "Edit Project Description"
        |> Modal.view


viewReadmeEmptyState : Session -> ProjectDetails -> Html Msg
viewReadmeEmptyState session project =
    let
        hasProjectAccess =
            Session.hasProjectAccess project.ref session

        cta =
            if hasProjectAccess then
                Button.iconThenLabel
                    ShowReadmeInstructionsModal
                    Icon.graduationCap
                    "Learn how to add a Project README"
                    |> Button.decorativeBlue

            else
                Button.iconThenLabel_
                    (Link.projectBranchRoot project.ref (Project.defaultBrowsingBranch project))
                    Icon.ability
                    "Browse Project Code"
    in
    EmptyState.iconCloud
        (EmptyState.CustomCenterPiece
            (div
                [ class "project-overview-page_empty-state_center-piece" ]
                [ ProjectRef.viewHashvatar project.ref ]
            )
        )
        |> EmptyState.withContent [ ProjectRef.view project.ref, cta |> Button.medium |> Button.view ]
        |> EmptyStateCard.view


viewReadmeCard : Session -> ProjectDetails -> WebData ( Maybe Readme, ReadmeCard.Model ) -> Html Msg
viewReadmeCard session project readme =
    case readme of
        NotAsked ->
            ReadmeCard.viewLoading

        Loading ->
            ReadmeCard.viewLoading

        Success ( Just rm, card ) ->
            Html.map ReadmeCardMsg (ReadmeCard.asCard card rm |> Card.asContained |> Card.view)

        Success ( Nothing, _ ) ->
            viewReadmeEmptyState session project

        Failure (Http.BadStatus 404) ->
            viewReadmeEmptyState session project

        Failure _ ->
            ReadmeCard.viewError RetryFetchReadme


viewDependencies : List ProjectDependency -> Html msg
viewDependencies deps =
    div [ class "project-dependencies" ] [ strong [] [ text "Dependencies" ], deps |> List.map ProjectDependency.toTag |> Tag.viewTags ]


view_ : Session -> ProjectDetails -> Model -> ( Maybe (List (Html Msg)), PageContent Msg )
view_ session project model =
    let
        viewKpi icon num labelSingular labelPlural kpiDescription =
            KpiTag.kpiTag labelSingular num
                |> KpiTag.withPlural labelPlural
                |> KpiTag.withIcon icon
                |> KpiTag.withTooltip
                    (Tooltip.text kpiDescription
                        |> Tooltip.tooltip
                        |> Tooltip.withArrow Tooltip.Middle
                        |> Tooltip.withPosition Tooltip.LeftOf
                    )
                |> KpiTag.view

        favKpi icon =
            viewKpi
                icon
                project.numFavs
                "Fav"
                "Faves"
                "Total number of favorites"

        fav =
            case ( session, project.isFaved ) of
                ( Session.SignedIn _, Project.NotFaved ) ->
                    Click.view [ class "not-faved" ]
                        [ favKpi Icon.heartOutline ]
                        (Click.onClick ToggleProjectFav)

                ( Session.SignedIn _, Project.Faved ) ->
                    Click.view [ class "is-faved" ]
                        [ favKpi Icon.heart ]
                        (Click.onClick ToggleProjectFav)

                ( Session.SignedIn _, Project.JustFaved ) ->
                    Click.view [ class "is-faved just-faved" ]
                        [ favKpi Icon.heart ]
                        (Click.onClick ToggleProjectFav)

                _ ->
                    favKpi Icon.heartOutline

        rightSide =
            [ div [ class "kpis" ]
                [ fav ]

            {- Turn off release downloads for now
               , viewKpi
                   Icon.download
                   (Project.fourWeekTotalDownloads project)
                   "Download"
                   "Downloads"
                   "Total downloads for the last 4 weeks"
               ]
            -}
            , Button.iconThenLabel ShowUseProjectModal Icon.download "Use Project"
                |> Button.medium
                |> Button.positive
                |> Button.view
            ]

        pageTitle =
            PageTitle.custom
                [ ProjectListing.projectListing project
                    |> ProjectListing.huge
                    |> ProjectListing.withClick Link.userProfile Link.projectOverview
                    |> ProjectListing.view
                ]
                |> PageTitle.withRightSide rightSide

        hasProjectAccess =
            Session.hasProjectAccess project.ref session

        showIfAccess c =
            if hasProjectAccess then
                Just c

            else
                Nothing

        edit icon label =
            Button.iconThenLabel ShowEditDescriptionModal icon label
                |> Button.small
                |> Button.outlined
                |> showIfAccess

        summaryAndTags =
            case ( project.summary, Set.toList project.tags ) of
                ( Just s, [] ) ->
                    Just
                        [ div [ class "project-description" ]
                            [ div [ class "project-summary" ] [ text s ]
                            , edit Icon.writingPad "Edit summary"
                                |> Maybe.map Button.view
                                |> Maybe.withDefault UI.nothing
                            ]
                        ]

                ( Just s, tags ) ->
                    Just
                        [ div [ class "project-description" ]
                            [ div [ class "project-summary" ] [ text s ]
                            , tags |> List.map Tag.tag |> Tag.viewTags
                            , Button.icon ShowEditDescriptionModal Icon.writingPad
                                |> Button.small
                                |> Button.outlined
                                |> showIfAccess
                                |> MaybeE.unwrap UI.nothing Button.view
                            ]
                        ]

                ( Nothing, [] ) ->
                    if hasProjectAccess then
                        Just
                            [ div [ class "project-description-empty-state" ]
                                [ text "Add a bit of detail with a summary."
                                , edit Icon.writingPad "Add now "
                                    |> MaybeE.unwrap UI.nothing Button.view
                                ]
                            ]

                    else
                        Nothing

                ( Nothing, tags ) ->
                    if hasProjectAccess then
                        Just
                            [ div []
                                [ div
                                    [ class "project-description-empty-state" ]
                                    [ text "Add a bit of detail with a summary."
                                    , edit Icon.writingPad "Add now "
                                        |> MaybeE.unwrap UI.nothing Button.view
                                    ]
                                , tags |> List.map Tag.tag |> Tag.viewTags
                                ]
                            ]

                    else
                        Just
                            [ div [] [ tags |> List.map Tag.tag |> Tag.viewTags ]
                            ]

        dependencies =
            model.dependencies
                |> RemoteData.toMaybe
                |> Maybe.map viewDependencies

        aside =
            case ( summaryAndTags, dependencies ) of
                ( Just sumAndTags, Just deps ) ->
                    Just (sumAndTags ++ [ deps ])

                ( Nothing, Just deps ) ->
                    Just [ deps ]

                ( Just sumAndTags, Nothing ) ->
                    Just sumAndTags

                _ ->
                    Nothing

        content =
            PageContent.oneColumn
                [ div
                    [ class "project-overview-page_layout" ]
                    [ div
                        [ class "project-overview-page_content" ]
                        [ viewReadmeCard session project model.readme ]
                    ]
                ]
                |> PageContent.withPageTitle pageTitle
    in
    ( aside, content )


view : Session -> ProjectRef -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view session projectRef project model =
    let
        modal =
            case model.modal of
                NoModal ->
                    Nothing

                ReadmeInstructionsModal ->
                    Just (viewReadmeInstructionsModal projectRef)

                EditDescriptionModal form ->
                    Just (viewEditDescriptionModal form)

                FinderModal fm ->
                    Just (Html.map FinderMsg (Finder.view fm))

        ( aside, content ) =
            view_ session project model
    in
    case aside of
        Just aside_ ->
            ( PageLayout.centeredLayout (PageContent.withLeftAside aside_ content)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Nothing ->
            ( PageLayout.centeredNarrowLayout content
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )
