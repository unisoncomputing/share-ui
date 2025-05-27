module UnisonShare.Page.ProjectReleasesPage exposing (..)

import Browser.Dom as Dom
import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Doc as Doc exposing (Doc)
import Code.Hash as Hash exposing (Hash)
import Code.Perspective as Perspective
import Code.ProjectSlug as ProjectSlug
import Code.Version as Version exposing (Version)
import Html exposing (Html, div, footer, h1, h2, header, p, section, strong, text)
import Html.Attributes exposing (class, classList, id)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.UserHandle as UserHandle
import Lib.Util as Util
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Button as Button exposing (Button)
import UI.ByAt as ByAt
import UI.Card as Card
import UI.Click as Click
import UI.CopyField as CopyField
import UI.Divider as Divider
import UI.EmptyState as EmptyState
import UI.EmptyStateCard as EmptyStateCard
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.CodeBrowsingContext as CodeBrowsingContext
import UnisonShare.InteractiveDoc as InteractiveDoc
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Project.Release as Release exposing (Release)
import UnisonShare.PublishProjectReleaseModal as PublishProjectReleaseModal
import UnisonShare.Route as Route
import UnisonShare.UcmCommand as UcmCommand



-- MODEL


type ProjectReleasesModal
    = NoModal
    | InstallModal Version
    | PublishReleaseModal PublishProjectReleaseModal.Model


type Releases
    = NoReleases
    | Releases
        { -- latest is a Maybe because even though we might have releases,
          -- latest has to be the latest _published_ release, and its possible
          -- to have a few unpublished releases, without a published one.
          latest : Maybe Release
        , past : List Release
        }


type alias ReleaseDraft =
    { branch : BranchSummary
    , version : Version
    }


type DocVisibility
    = Unknown
    | Cropped
    | NotCropped
    | MadeFullyVisible


type alias ReleaseNotes =
    { doc : Doc
    , interactiveDoc : InteractiveDoc.Model
    , docVisibility : DocVisibility
    }


type alias Model =
    { releases : WebData Releases
    , latestReleaseNotes : WebData (Maybe ReleaseNotes)
    , releaseDrafts : WebData (List ReleaseDraft)
    , modal : ProjectReleasesModal
    }


init : AppContext -> ProjectRef -> WebData (Maybe Version) -> ( Model, Cmd Msg )
init appContext projectRef latestVersion =
    let
        ( latestReleaseNotes, latestReleaseNotesCmd ) =
            case latestVersion of
                Success (Just v) ->
                    ( Loading
                    , fetchReleaseNotes
                        FetchLatestReleaseNotesFinished
                        appContext
                        projectRef
                        v
                    )

                Success Nothing ->
                    ( Success Nothing, Cmd.none )

                _ ->
                    ( NotAsked, Cmd.none )
    in
    ( { releases = Loading
      , latestReleaseNotes = latestReleaseNotes
      , releaseDrafts = Loading
      , modal = NoModal
      }
    , Cmd.batch
        [ fetchProjectReleases appContext projectRef
        , fetchReleaseDrafts appContext projectRef
        , latestReleaseNotesCmd
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchProjectReleasesFinished (WebData (List Release))
    | FetchReleaseDraftsFinished (WebData (List BranchSummary))
    | FetchLatestReleaseNotesFinished (WebData (Maybe Doc))
    | ShowPublishReleaseModal (Maybe BranchSummary)
    | ShowInstallModal Version
    | CloseModal
    | InteractiveDocMsg InteractiveDoc.Msg
    | PublishProjectReleaseModalMsg PublishProjectReleaseModal.Msg
    | IsReleaseNotesCropped (Result Dom.Error Bool)
    | ShowFullReleaseNotes


type OutMsg
    = None
    | PublishedNewRelease Release


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case msg of
        FetchProjectReleasesFinished data ->
            ( { model | releases = RemoteData.map toProjectReleases data }
            , Cmd.none
            , None
            )

        FetchReleaseDraftsFinished draftBranches ->
            let
                toDraft branchSummary acc =
                    case branchSummary.ref of
                        BranchRef.ReleaseDraftBranchRef v ->
                            { version = v, branch = branchSummary } :: acc

                        _ ->
                            acc

                toDrafts brs =
                    brs
                        |> List.foldl toDraft []
                        |> Util.sortByWith .version Version.descending
            in
            ( { model | releaseDrafts = RemoteData.map toDrafts draftBranches }, Cmd.none, None )

        FetchLatestReleaseNotesFinished doc ->
            let
                releaseNotes =
                    RemoteData.map
                        (Maybe.map (\d -> { doc = d, interactiveDoc = InteractiveDoc.init, docVisibility = Unknown }))
                        doc
            in
            ( { model | latestReleaseNotes = releaseNotes }, isReleaseNotesCropped, None )

        ShowPublishReleaseModal draftBranch ->
            case model.releases of
                Success rs ->
                    let
                        ( modal_, cmd ) =
                            PublishProjectReleaseModal.init
                                appContext
                                projectRef
                                (currentVersion rs)
                                draftBranch
                    in
                    ( { model | modal = PublishReleaseModal modal_ }
                    , Cmd.map PublishProjectReleaseModalMsg cmd
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        ShowInstallModal projectVersion ->
            ( { model | modal = InstallModal projectVersion }, Cmd.none, None )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, None )

        PublishProjectReleaseModalMsg pprmMsg ->
            case model.modal of
                PublishReleaseModal m ->
                    let
                        ( m_, cmd, out ) =
                            PublishProjectReleaseModal.update appContext projectRef pprmMsg m

                        ( newModel, notesCmd, releasesOut ) =
                            case out of
                                PublishProjectReleaseModal.NoOutMsg ->
                                    ( { model | modal = PublishReleaseModal m_ }, Cmd.none, None )

                                PublishProjectReleaseModal.Published latest ->
                                    let
                                        past_ rs =
                                            case rs of
                                                NoReleases ->
                                                    []

                                                Releases rs_ ->
                                                    case rs_.latest of
                                                        Just r ->
                                                            r :: rs_.past

                                                        _ ->
                                                            rs_.past

                                        past =
                                            model.releases
                                                |> RemoteData.map past_
                                                |> RemoteData.withDefault []

                                        releases =
                                            { latest = Just latest
                                            , past = past
                                            }
                                    in
                                    ( { model | releases = Success (Releases releases), modal = PublishReleaseModal m_ }
                                    , fetchReleaseNotes FetchLatestReleaseNotesFinished appContext projectRef latest.version
                                    , PublishedNewRelease latest
                                    )

                                PublishProjectReleaseModal.RequestCloseModal ->
                                    ( { model | modal = NoModal }, Cmd.none, None )
                    in
                    ( newModel, Cmd.batch [ Cmd.map PublishProjectReleaseModalMsg cmd, notesCmd ], releasesOut )

                _ ->
                    ( model, Cmd.none, None )

        InteractiveDocMsg dMsg ->
            case ( model.releases, model.latestReleaseNotes ) of
                ( Success (Releases releases), Success (Just rn) ) ->
                    case releases.latest of
                        Just latest ->
                            let
                                config =
                                    AppContext.toCodeConfig
                                        appContext
                                        (CodeBrowsingContext.project projectRef (Release.branchRef latest))
                                        Perspective.relativeRootPerspective

                                ( interactiveDoc, cmd, iOutMsg ) =
                                    InteractiveDoc.update config dMsg rn.interactiveDoc

                                navCmd =
                                    case iOutMsg of
                                        InteractiveDoc.OpenDefinition ref ->
                                            Route.navigate
                                                appContext.navKey
                                                (Route.projectBranchDefinition projectRef
                                                    (Release.branchRef latest)
                                                    Perspective.relativeRootPerspective
                                                    ref
                                                )

                                        _ ->
                                            Cmd.none
                            in
                            ( { model
                                | latestReleaseNotes =
                                    Success (Just { rn | interactiveDoc = interactiveDoc })
                              }
                            , Cmd.batch [ Cmd.map InteractiveDocMsg cmd, navCmd ]
                            , None
                            )

                        _ ->
                            ( model, Cmd.none, None )

                _ ->
                    ( model, Cmd.none, None )

        IsReleaseNotesCropped r ->
            let
                visibility =
                    case r of
                        Ok True ->
                            Cropped

                        Ok False ->
                            NotCropped

                        -- If we can't tell, better make it fully visible, than Unknown
                        Err _ ->
                            MadeFullyVisible
            in
            ( { model
                | latestReleaseNotes =
                    RemoteData.map
                        (Maybe.map (\rn -> { rn | docVisibility = visibility }))
                        model.latestReleaseNotes
              }
            , Cmd.none
            , None
            )

        ShowFullReleaseNotes ->
            ( { model
                | latestReleaseNotes =
                    RemoteData.map
                        (Maybe.map (\rn -> { rn | docVisibility = MadeFullyVisible }))
                        model.latestReleaseNotes
              }
            , Cmd.none
            , None
            )

        NoOp ->
            ( model, Cmd.none, None )


updateWithNoLatestReleaseNotes : Model -> Model
updateWithNoLatestReleaseNotes model =
    { model | latestReleaseNotes = Success Nothing }



-- HELPERS


currentVersion : Releases -> Version
currentVersion releases =
    let
        unreleased =
            Version.empty
    in
    case releases of
        NoReleases ->
            unreleased

        Releases rs ->
            rs.latest
                |> Maybe.map .version
                |> Maybe.withDefault unreleased


toProjectReleases : List Release -> Releases
toProjectReleases releases =
    let
        sort =
            Util.sortByWith .version Version.descending
    in
    case sort releases of
        latest :: past ->
            if Release.isPublished latest then
                Releases
                    { latest = Just latest
                    , past = past
                    }

            else
                Releases
                    { latest = Nothing
                    , past = latest :: past
                    }

        _ ->
            NoReleases



-- EFFECTS


isReleaseNotesCropped : Cmd Msg
isReleaseNotesCropped =
    Dom.getViewportOf "release-notes_container"
        |> Task.map (\v -> v.viewport.height < v.scene.height)
        |> Task.attempt IsReleaseNotesCropped


fetchProjectReleases : AppContext -> ProjectRef -> Cmd Msg
fetchProjectReleases appContext projectRef =
    ShareApi.projectReleases projectRef
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list Release.decode))
            (RemoteData.fromResult >> FetchProjectReleasesFinished)
        |> HttpApi.perform appContext.api


fetchReleaseDrafts : AppContext -> ProjectRef -> Cmd Msg
fetchReleaseDrafts appContext projectRef =
    let
        params =
            { kind = ShareApi.ProjectBranches
            , searchQuery = Just "releases/drafts/"
            , limit = 10
            , nextCursor = Nothing
            , prevCursor = Nothing
            }
    in
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list BranchSummary.decode))
            (RemoteData.fromResult >> FetchReleaseDraftsFinished)
        |> HttpApi.perform appContext.api


fetchLatestReleaseNotesAndUpdate : AppContext -> ProjectRef -> Model -> Version -> ( Model, Cmd Msg )
fetchLatestReleaseNotesAndUpdate appContext projectRef model version =
    ( { model | latestReleaseNotes = Loading }
    , fetchReleaseNotes FetchLatestReleaseNotesFinished appContext projectRef version
    )


fetchReleaseNotes : (WebData (Maybe Doc) -> Msg) -> AppContext -> ProjectRef -> Version -> Cmd Msg
fetchReleaseNotes doneMsg appContext projectRef version =
    ShareApi.projectReleaseNotes projectRef version
        |> HttpApi.toRequest
            (Decode.field "doc" (Decode.nullable Doc.decode))
            (RemoteData.fromResult >> doneMsg)
        |> HttpApi.perform appContext.api



-- VIEW


pageTitle : ProjectRef -> Maybe (Button Msg) -> PageTitle Msg
pageTitle projectRef action =
    let
        rightSide =
            case action of
                Just a ->
                    [ Button.view a ]

                Nothing ->
                    []

        pageDescription =
            "Explore latest and past releases of " ++ ProjectRef.toString projectRef
    in
    PageTitle.title "Releases"
        |> PageTitle.withDescription pageDescription
        |> PageTitle.withRightSide rightSide


viewLoadingPage : ProjectRef -> PageLayout Msg
viewLoadingPage projectRef =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        content =
            PageContent.oneColumn
                [ div [ class "project-releases_releases" ]
                    [ Card.card
                        [ shape Placeholder.Large
                        , shape Placeholder.Small
                        , shape Placeholder.Medium
                        ]
                        |> Card.asContained
                        |> Card.view
                    , div [ class "project-releases_loading_past-releases" ]
                        [ shape Placeholder.Large
                        , shape Placeholder.Small
                        , shape Placeholder.Medium
                        , shape Placeholder.Large
                        ]
                    ]
                ]
                |> PageContent.withPageTitle (pageTitle projectRef Nothing)
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewErrorPage : ProjectRef -> Http.Error -> PageLayout Msg
viewErrorPage projectRef _ =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ Card.card
                [ StatusBanner.bad "Something broke on our end and we couldn't show the project releases. Please try again."
                ]
                |> Card.withClassName "project-releases_error"
                |> Card.asContained
                |> Card.view
            ]
            |> PageContent.withPageTitle (pageTitle projectRef Nothing)
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewEmptyState : ProjectRef -> List (Html msg)
viewEmptyState projectRef =
    let
        emptyState =
            EmptyState.iconCloud (EmptyState.CircleCenterPiece (text "🐣"))
                |> EmptyState.withContent
                    [ p
                        [ class "no-releases" ]
                        [ text "There are no "
                        , strong [] [ text (ProjectRef.toString projectRef) ]
                        , text " releases yet."
                        ]
                    , p [] [ text "Check back later." ]
                    ]
    in
    [ EmptyStateCard.view emptyState
    ]


viewLatestReleaseNotes : ReleaseNotes -> Html Msg
viewLatestReleaseNotes releaseNotes =
    let
        ( showFullDoc, shownInFull ) =
            case releaseNotes.docVisibility of
                Unknown ->
                    ( UI.nothing, False )

                Cropped ->
                    ( div [ class "show-full-release-notes" ]
                        [ Button.iconThenLabel ShowFullReleaseNotes Icon.arrowDown "Show full release notes"
                            |> Button.small
                            |> Button.view
                        ]
                    , False
                    )

                _ ->
                    ( UI.nothing, True )

        classes =
            classList
                [ ( "project-release-details_release-notes", True )
                , ( "shown-in-full", shownInFull )
                ]
    in
    section [ classes ]
        [ div [ id "release-notes_container" ]
            [ Html.map InteractiveDocMsg
                (InteractiveDoc.view
                    releaseNotes.interactiveDoc
                    releaseNotes.doc
                )
            ]
        , showFullDoc
        ]


viewLatestRelease : AppContext -> ProjectRef -> Release -> Maybe ReleaseNotes -> Html Msg
viewLatestRelease appContext projectRef release releaseNotes =
    let
        doc =
            releaseNotes
                |> Maybe.map viewLatestReleaseNotes
                |> Maybe.withDefault UI.nothing

        byAt =
            case release.status of
                Release.Published p ->
                    ByAt.view
                        appContext.timeZone
                        appContext.now
                        (ByAt.byAt p.by p.at)

                _ ->
                    UI.nothing
    in
    Card.card
        [ header []
            [ div [ class "project-release-details_version-hash" ]
                [ h1 []
                    [ Click.view []
                        [ Version.view release.version ]
                        (Link.projectRelease projectRef release.version)
                    ]
                , viewBranchHash
                    release.projectRef
                    (Release.branchRef release)
                    release.causalHashSquashed
                ]
            , byAt
            ]
        , doc
        , footer []
            [ Button.iconThenLabel_ (Link.projectBranchRoot release.projectRef (Release.branchRef release)) Icon.browse "Browse"
                |> Button.medium
                |> Button.view
            , Button.iconThenLabel (ShowInstallModal release.version) Icon.download "Install"
                |> Button.positive
                |> Button.medium
                |> Button.view
            ]
        ]
        |> Card.asContained
        |> Card.withClassName "project-release-details"
        |> Card.view


viewPastReleases : AppContext -> ProjectRef -> List Release -> List (Html msg)
viewPastReleases appContext projectRef releases =
    let
        status r =
            case r.status of
                Release.Published _ ->
                    Tag.tag "Published"
                        |> Tag.view

                Release.Unpublished _ ->
                    Tag.tag "Unpublished"
                        |> Tag.view

                _ ->
                    UI.nothing

        byAt r =
            case r.status of
                Release.Published p ->
                    ByAt.view
                        appContext.timeZone
                        appContext.now
                        (ByAt.byAt p.by p.at)

                Release.Unpublished p ->
                    ByAt.view
                        appContext.timeZone
                        appContext.now
                        (ByAt.byAt p.by p.at)

                _ ->
                    UI.nothing

        viewRelease r =
            div [ class "project-releases_past-releases_release" ]
                [ div [ class "project-releases_past-releases_release_version-and-hash" ]
                    [ Click.view [] [ Version.view r.version ] (Link.projectRelease projectRef r.version)
                    , viewBranchHash r.projectRef (Release.branchRef r) r.causalHashSquashed
                    ]
                , div [ class "project-releases_past-releases_release_status-and-by-at" ]
                    [ status r, byAt r ]
                ]
    in
    [ div [ class "project-releases_past-releases_group" ]
        [ header []
            [ h2 [] [ text "Past Releases" ]
            , Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view
            ]
        , div [ class "project-releases_past-releases" ]
            (List.map viewRelease releases)
        ]
    ]


{-| TODO: should this be in ui-core?
-}
viewBranchHash : ProjectRef -> BranchRef -> Hash -> Html msg
viewBranchHash projectRef branchRef hash =
    let
        link =
            Link.projectBranchRoot projectRef branchRef
    in
    Click.view
        [ class "browsable-branch-hash" ]
        [ Hash.view hash, Icon.view Icon.browse ]
        link


viewDraft : ReleaseDraft -> Html Msg
viewDraft draft =
    let
        br =
            draft.branch
    in
    div [ class "release-draft" ]
        [ div [ class "release-draft_meta" ]
            [ Icon.view Icon.writingPad
            , Version.view draft.version
            , text "Release Draft"
            , BranchRef.toTag br.ref |> Tag.view
            , viewBranchHash br.project.ref br.ref br.causalHash
            ]
        , Button.iconThenLabel (ShowPublishReleaseModal (Just br)) Icon.rocket "Publish"
            |> Button.emphasized
            |> Button.small
            |> Button.view
        ]


viewPageContent :
    AppContext
    -> ProjectDetails
    -> Releases
    -> Maybe ReleaseNotes
    -> List ReleaseDraft
    -> PageContent Msg
viewPageContent appContext project releases latestReleaseNotes releaseDrafts =
    let
        content =
            case releases of
                NoReleases ->
                    viewEmptyState project.ref

                Releases rs ->
                    case rs.latest of
                        Just l ->
                            if List.isEmpty rs.past then
                                [ viewLatestRelease appContext project.ref l latestReleaseNotes ]

                            else
                                viewLatestRelease appContext project.ref l latestReleaseNotes
                                    :: viewPastReleases appContext project.ref rs.past

                        Nothing ->
                            viewPastReleases appContext project.ref rs.past

        currentVersion_ =
            currentVersion releases

        ( drafts, publishAction ) =
            if Project.canMaintain project then
                ( releaseDrafts
                    |> List.filter (.version >> Version.lessThan currentVersion_)
                    |> NEL.fromList
                    |> Maybe.map (NEL.map viewDraft)
                    |> Maybe.map NEL.toList
                    |> Maybe.map Card.card
                    |> Maybe.map (Card.withClassName "project-releases_drafts")
                    |> Maybe.map Card.asContained
                    |> Maybe.map Card.withTightPadding
                    |> MaybeE.unwrap UI.nothing Card.view
                , Button.iconThenLabel (ShowPublishReleaseModal Nothing) Icon.rocket "Cut a new release"
                    |> Button.outlined
                    |> Button.small
                    |> Just
                )

            else
                ( UI.nothing, Nothing )
    in
    PageContent.oneColumn
        [ div [ class "project-releases_releases" ] (drafts :: content) ]
        |> PageContent.withPageTitle (pageTitle project.ref publishAction)


viewInstallModal : ProjectRef -> Version -> Html Msg
viewInstallModal projectRef version =
    let
        projectRef_ =
            ProjectRef.toString projectRef

        libVersion =
            UserHandle.toUnprefixedString (ProjectRef.handle projectRef)
                ++ "_"
                ++ ProjectSlug.toNamespaceString (ProjectRef.slug projectRef)
                ++ "_"
                ++ Version.toNamespaceString version

        installCommand =
            UcmCommand.Install projectRef (Just (BranchRef.releaseBranchRef version))
                |> UcmCommand.toString

        content =
            Modal.Content
                (div [ class "instruction" ]
                    [ p []
                        [ text "From within your project in UCM, run the "
                        , strong [] [ text "lib.install" ]
                        , text " command:"
                        ]
                    , CopyField.copyField (\_ -> NoOp) installCommand |> CopyField.withPrefix "myProject/main>" |> CopyField.view
                    , div [ class "hint" ] [ text "Copy and paste this command into UCM." ]
                    , div [ class "upgrade-hint" ]
                        [ div [ class "upgrade-icon" ] [ Icon.view Icon.arrowUp ]
                        , div [ class "upgrade-hint_content" ]
                            [ div [] [ text "Upgrading from a previous version? Install using the above and then run:" ]
                            , div [ class "monospace" ] [ text ("myProject/main> upgrade <old_version> " ++ libVersion) ]
                            ]
                        ]
                    , Divider.divider |> Divider.small |> Divider.view
                    , div [ class "action" ] [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" |> Button.emphasized |> Button.view ]
                    ]
                )
    in
    Modal.modal "use-project-modal" CloseModal content
        |> Modal.withHeader ("Install " ++ projectRef_)
        |> Modal.view


view : AppContext -> ProjectDetails -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project model =
    let
        data =
            RemoteData.map3 (\rs rn drafts_ -> ( rs, rn, drafts_ ))
                model.releases
                model.latestReleaseNotes
                model.releaseDrafts
    in
    case data of
        NotAsked ->
            ( viewLoadingPage project.ref, Nothing )

        Loading ->
            ( viewLoadingPage project.ref, Nothing )

        Success ( rs, latestReleaseNotes, drafts_ ) ->
            let
                modal =
                    case model.modal of
                        NoModal ->
                            Nothing

                        InstallModal v ->
                            Just (viewInstallModal project.ref v)

                        PublishReleaseModal pprm ->
                            Just
                                (Html.map
                                    PublishProjectReleaseModalMsg
                                    (PublishProjectReleaseModal.view (currentVersion rs) pprm)
                                )
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext project rs latestReleaseNotes drafts_)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure e ->
            ( viewErrorPage project.ref e, Nothing )
