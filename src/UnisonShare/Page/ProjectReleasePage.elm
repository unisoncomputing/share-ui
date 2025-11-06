module UnisonShare.Page.ProjectReleasePage exposing (..)

import Browser.Dom as Dom
import Code.BranchRef as BranchRef
import Code.Definition.Doc as Doc exposing (Doc)
import Code.Hash as Hash
import Code.Perspective as Perspective
import Code.Version as Version exposing (Version)
import Html exposing (Html, div, p, section, span, strong, text)
import Html.Attributes exposing (class, classList, id)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.CopyField as CopyField
import UI.Divider as Divider
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle exposing (PageTitle)
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.InteractiveDoc as InteractiveDoc
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Project.Release as Release exposing (Release)
import UnisonShare.Route as Route
import UnisonShare.UcmCommand as UcmCommand



-- MODEL


type ProjectReleaseModal
    = NoModal
    | InstallModal Version


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
    { release : WebData Release
    , releaseNotes : WebData (Maybe ReleaseNotes)
    , modal : ProjectReleaseModal
    }


init : AppContext -> ProjectRef -> Version -> ( Model, Cmd Msg )
init appContext projectRef version =
    ( { release = Loading
      , releaseNotes = Loading
      , modal = NoModal
      }
    , Cmd.batch
        [ fetchRelease appContext projectRef version
        , fetchReleaseNotes appContext projectRef version
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | FetchReleaseFinished (WebData Release)
    | FetchReleaseNotesFinished (WebData (Maybe Doc))
    | ShowInstallModal Version
    | CloseModal
    | InteractiveDocMsg InteractiveDoc.Msg
    | IsReleaseNotesCropped (Result Dom.Error Bool)
    | ShowFullReleaseNotes


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef msg model =
    case msg of
        FetchReleaseFinished release ->
            ( { model | release = release }, Cmd.none )

        FetchReleaseNotesFinished doc ->
            let
                releaseNotes =
                    RemoteData.map
                        (Maybe.map (\d -> { doc = d, interactiveDoc = InteractiveDoc.init, docVisibility = Unknown }))
                        doc
            in
            ( { model | releaseNotes = releaseNotes }, isReleaseNotesCropped )

        ShowInstallModal projectVersion ->
            ( { model | modal = InstallModal projectVersion }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )

        InteractiveDocMsg dMsg ->
            case ( model.release, model.releaseNotes ) of
                ( Success release, Success (Just rn) ) ->
                    let
                        config =
                            AppContext.toCodeConfig
                                appContext
                                { projectRef = projectRef, branchRef = Release.branchRef release }
                                Perspective.relativeRootPerspective

                        ( interactiveDoc, cmd, iOutMsg ) =
                            InteractiveDoc.update config dMsg rn.interactiveDoc

                        navCmd =
                            case iOutMsg of
                                InteractiveDoc.OpenDefinition ref ->
                                    Route.navigate
                                        appContext.navKey
                                        (Route.projectBranchDefinition projectRef
                                            (Release.branchRef release)
                                            Perspective.relativeRootPerspective
                                            ref
                                        )

                                _ ->
                                    Cmd.none
                    in
                    ( { model
                        | releaseNotes =
                            Success (Just { rn | interactiveDoc = interactiveDoc })
                      }
                    , Cmd.batch [ Cmd.map InteractiveDocMsg cmd, navCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

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
                | releaseNotes =
                    RemoteData.map
                        (Maybe.map (\rn -> { rn | docVisibility = visibility }))
                        model.releaseNotes
              }
            , Cmd.none
            )

        ShowFullReleaseNotes ->
            ( { model
                | releaseNotes =
                    RemoteData.map
                        (Maybe.map (\rn -> { rn | docVisibility = MadeFullyVisible }))
                        model.releaseNotes
              }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



-- EFFECTS


isReleaseNotesCropped : Cmd Msg
isReleaseNotesCropped =
    Dom.getViewportOf "release-notes_container"
        |> Task.map (\v -> v.viewport.height < v.scene.height)
        |> Task.attempt IsReleaseNotesCropped


fetchRelease : AppContext -> ProjectRef -> Version -> Cmd Msg
fetchRelease appContext projectRef version =
    ShareApi.projectRelease projectRef version
        |> HttpApi.toRequest Release.decode (RemoteData.fromResult >> FetchReleaseFinished)
        |> HttpApi.perform appContext.api


fetchReleaseNotes : AppContext -> ProjectRef -> Version -> Cmd Msg
fetchReleaseNotes appContext projectRef version =
    ShareApi.projectReleaseNotes projectRef version
        |> HttpApi.toRequest
            (Decode.field "doc" (Decode.nullable Doc.decode))
            (RemoteData.fromResult >> FetchReleaseNotesFinished)
        |> HttpApi.perform appContext.api



-- VIEW


pageTitle : Version -> PageTitle Msg
pageTitle version =
    PageTitle.title ("Release " ++ Version.toString version)


detailedPageTitle : AppContext -> Release -> PageTitle Msg
detailedPageTitle appContext release =
    let
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
    pageTitle release.version
        |> PageTitle.withDescription_
            (span [ class "project-release_page-title_description" ]
                [ Hash.view release.causalHashSquashed, byAt ]
            )
        |> PageTitle.withRightSide
            [ div [ class "project-release_actions" ]
                [ Button.iconThenLabel_ (Link.projectBranchRoot release.projectRef (Release.branchRef release)) Icon.browse "Browse Code"
                    |> Button.medium
                    |> Button.view
                , Button.iconThenLabel (ShowInstallModal release.version) Icon.download "Install"
                    |> Button.positive
                    |> Button.medium
                    |> Button.view
                ]
            ]


viewLoadingPage : Version -> PageLayout Msg
viewLoadingPage version =
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
                    ]
                ]
                |> PageContent.withPageTitle (pageTitle version)
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewErrorPage : Version -> Http.Error -> PageLayout Msg
viewErrorPage version _ =
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ Card.card
                [ StatusBanner.bad "Something broke on our end and we couldn't show the project release. Please try again."
                ]
                |> Card.withClassName "project-releases_error"
                |> Card.asContained
                |> Card.view
            ]
            |> PageContent.withPageTitle (pageTitle version)
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewReleaseNotes : ReleaseNotes -> Html Msg
viewReleaseNotes releaseNotes =
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


viewPageContent : AppContext -> Release -> Maybe ReleaseNotes -> PageContent Msg
viewPageContent appContext release releaseNotes =
    let
        content =
            case releaseNotes of
                Just rn ->
                    [ Card.card
                        [ viewReleaseNotes rn
                        ]
                        |> Card.asContained
                        |> Card.view
                    ]

                Nothing ->
                    []
    in
    PageContent.oneColumn content
        |> PageContent.withPageTitle (detailedPageTitle appContext release)


viewInstallModal : ProjectRef -> Version -> Html Msg
viewInstallModal projectRef version =
    let
        projectRef_ =
            ProjectRef.toString projectRef

        installCommand =
            UcmCommand.Install projectRef (Just (BranchRef.releaseBranchRef version))
                |> UcmCommand.toString

        content =
            Modal.Content
                (div [ class "use-project-modal-content" ]
                    [ p []
                        [ text "From within your project in UCM, run the "
                        , strong [] [ text "lib.install" ]
                        , text " command:"
                        ]
                    , CopyField.copyField (\_ -> NoOp) installCommand |> CopyField.withPrefix "myProject/main>" |> CopyField.view
                    , div [ class "hint" ] [ text "Copy and paste this command into UCM." ]
                    , Divider.divider |> Divider.small |> Divider.view
                    , div [ class "action" ] [ Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" |> Button.emphasized |> Button.view ]
                    ]
                )
    in
    Modal.modal "use-project-modal" CloseModal content
        |> Modal.withHeader ("Install " ++ projectRef_)
        |> Modal.view


view : AppContext -> ProjectRef -> Version -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext projectRef version model =
    let
        data =
            RemoteData.map2 (\rl rn -> ( rl, rn ))
                model.release
                model.releaseNotes
    in
    case data of
        NotAsked ->
            ( viewLoadingPage version, Nothing )

        Loading ->
            ( viewLoadingPage version, Nothing )

        Success ( release, releaseNotes ) ->
            let
                modal =
                    case model.modal of
                        NoModal ->
                            Nothing

                        InstallModal v ->
                            Just (viewInstallModal projectRef v)
            in
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext release releaseNotes)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , modal
            )

        Failure e ->
            ( viewErrorPage version e, Nothing )
