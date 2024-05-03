module UnisonShare.PublishProjectReleaseModal exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Doc as Doc exposing (Doc)
import Code.Perspective as Perspective
import Code.Version as Version exposing (Version)
import Html exposing (Html, div, footer, h1, h3, header, p, section, small, strong, text)
import Html.Attributes exposing (class, classList)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import List.Nonempty as NEL
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Form.RadioField as RadioField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.Placeholder as Placeholder
import UI.StatusBanner as StatusBanner
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext as AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.CodeBrowsingContext as CodeBrowsingContext
import UnisonShare.InteractiveDoc as InteractiveDoc
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Project.Release as Release exposing (Release)
import UnisonShare.SearchBranchSheet as SearchBranchSheet


type alias SelectBranchOpenSheet =
    { sheet : SearchBranchSheet.Model }


type SelectBranchSheet
    = Open SelectBranchOpenSheet
    | Closed


type alias ReleaseNotes =
    { doc : Doc, interactiveDoc : InteractiveDoc.Model, maximized : Bool }


type alias SourceBranch =
    { branchRef : BranchRef
    , branch : WebData BranchSummary
    , releaseNotes : WebData (Maybe ReleaseNotes)
    }


type alias Model =
    { sourceBranch : SourceBranch
    , version : Version
    , selectBranchSheet : SelectBranchSheet
    , publishedRelease : WebData Release
    , latestBranches : WebData (List BranchSummary)
    }


init : AppContext -> ProjectRef -> Version -> Maybe BranchSummary -> ( Model, Cmd Msg )
init appContext projectRef current draftBranch =
    case draftBranch of
        Just b ->
            let
                version =
                    case b.ref of
                        BranchRef.ReleaseDraftBranchRef v ->
                            Version.clampToNextValid current v

                        _ ->
                            Version.nextMajor current
            in
            ( { sourceBranch =
                    { branchRef = b.ref, branch = Success b, releaseNotes = Loading }
              , version = version
              , selectBranchSheet = Closed
              , publishedRelease = RemoteData.NotAsked
              , latestBranches = RemoteData.Loading
              }
            , Cmd.batch
                [ fetchBranchReleaseNotes appContext projectRef b.ref
                , fetchLatestBranches appContext projectRef
                ]
            )

        Nothing ->
            ( { sourceBranch =
                    { branchRef = BranchRef.main_, branch = Loading, releaseNotes = Loading }
              , version = Version.nextMajor current
              , selectBranchSheet = Closed
              , publishedRelease = RemoteData.NotAsked
              , latestBranches = RemoteData.Loading
              }
            , Cmd.batch
                [ fetchMainBranch appContext projectRef
                , fetchBranchReleaseNotes appContext projectRef BranchRef.main_
                , fetchLatestBranches appContext projectRef
                ]
            )



-- UPDATE


type Msg
    = CloseModal
    | FetchMainBranchFinished (WebData BranchSummary)
    | FetchReleaseNotesFinished BranchRef (HttpResult (Maybe Doc))
    | FetchLatestBranchesFinished (WebData (List BranchSummary))
    | ToggleSelectBranchSheet
    | UpdateVersion Version
    | SearchBranchSheetMsg SearchBranchSheet.Msg
    | Publish
    | PublishFinished (WebData Release)
    | ToggleReleaseNotesSize
    | InteractiveDocMsg InteractiveDoc.Msg


type OutMsg
    = NoOutMsg
    | RequestCloseModal
    | Published Release


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case msg of
        FetchMainBranchFinished mainBranch ->
            if BranchRef.equals model.sourceBranch.branchRef BranchRef.main_ then
                let
                    sourceBranch =
                        model.sourceBranch

                    sourceBranch_ =
                        { sourceBranch | branch = mainBranch }
                in
                ( { model | sourceBranch = sourceBranch_ }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        FetchLatestBranchesFinished latestBranches ->
            ( { model | latestBranches = latestBranches }, Cmd.none, NoOutMsg )

        FetchReleaseNotesFinished branchRef doc ->
            if BranchRef.equals model.sourceBranch.branchRef branchRef then
                let
                    sourceBranch =
                        model.sourceBranch

                    sourceBranch_ rn =
                        { sourceBranch | releaseNotes = rn }
                in
                case doc of
                    Ok d ->
                        ( { model | sourceBranch = sourceBranch_ (Success (Maybe.map releaseNotes d)) }, Cmd.none, NoOutMsg )

                    Err (Http.BadStatus 404) ->
                        ( { model | sourceBranch = sourceBranch_ (Success Nothing) }, Cmd.none, NoOutMsg )

                    Err e ->
                        ( { model | sourceBranch = sourceBranch_ (Failure e) }, Cmd.none, NoOutMsg )

            else
                ( model, Cmd.none, NoOutMsg )

        CloseModal ->
            ( model, Cmd.none, RequestCloseModal )

        UpdateVersion version ->
            ( { model | version = version }, Cmd.none, NoOutMsg )

        ToggleSelectBranchSheet ->
            let
                selectBranchSheet =
                    case model.selectBranchSheet of
                        Open _ ->
                            Closed

                        Closed ->
                            Open { sheet = SearchBranchSheet.init ShareApi.ProjectBranches }
            in
            ( { model | selectBranchSheet = selectBranchSheet }
            , Cmd.none
            , NoOutMsg
            )

        SearchBranchSheetMsg sbsMsg ->
            case model.selectBranchSheet of
                Open s ->
                    let
                        ( sheet_, cmd, sbsOut ) =
                            SearchBranchSheet.update appContext projectRef sbsMsg s.sheet

                        ( newModel, releaseNotesCmd ) =
                            case sbsOut of
                                SearchBranchSheet.NoOutMsg ->
                                    ( { model | selectBranchSheet = Open { s | sheet = sheet_ } }, Cmd.none )

                                SearchBranchSheet.SelectBranchRequest branch ->
                                    let
                                        sourceBranch =
                                            model.sourceBranch

                                        sourceBranch_ =
                                            { sourceBranch
                                                | branchRef = branch.ref
                                                , branch = Success branch
                                                , releaseNotes = Loading
                                            }
                                    in
                                    ( { model | sourceBranch = sourceBranch_, selectBranchSheet = Closed }, fetchBranchReleaseNotes appContext projectRef branch.ref )
                    in
                    ( newModel
                    , Cmd.batch [ Cmd.map SearchBranchSheetMsg cmd, releaseNotesCmd ]
                    , NoOutMsg
                    )

                Closed ->
                    ( model, Cmd.none, NoOutMsg )

        Publish ->
            case model.sourceBranch.branch of
                Success br ->
                    ( { model | publishedRelease = Loading }
                    , publishProjectRelease appContext projectRef br model.version
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        PublishFinished release ->
            let
                ( cmd, out ) =
                    case release of
                        Success r ->
                            ( Util.delayMsg 1500 CloseModal, Published r )

                        _ ->
                            ( Cmd.none, NoOutMsg )
            in
            ( { model | publishedRelease = release }, cmd, out )

        ToggleReleaseNotesSize ->
            case model.sourceBranch.releaseNotes of
                Success (Just rn) ->
                    let
                        sourceBranch =
                            model.sourceBranch

                        sourceBranch_ =
                            { sourceBranch | releaseNotes = Success (Just { rn | maximized = not rn.maximized }) }
                    in
                    ( { model | sourceBranch = sourceBranch_ }
                    , Cmd.none
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )

        InteractiveDocMsg dMsg ->
            case model.sourceBranch.releaseNotes of
                Success (Just rn) ->
                    let
                        config =
                            AppContext.toCodeConfig
                                appContext
                                (CodeBrowsingContext.project projectRef model.sourceBranch.branchRef)
                                Perspective.relativeRootPerspective

                        -- TODO: Should we allow navigation from release notes?
                        ( id, idCmd, _ ) =
                            InteractiveDoc.update config dMsg rn.interactiveDoc

                        sourceBranch =
                            model.sourceBranch

                        sourceBranch_ =
                            { sourceBranch | releaseNotes = Success (Just { rn | interactiveDoc = id }) }
                    in
                    ( { model | sourceBranch = sourceBranch_ }
                    , Cmd.map InteractiveDocMsg idCmd
                    , NoOutMsg
                    )

                _ ->
                    ( model, Cmd.none, NoOutMsg )



-- EFFECTS


fetchMainBranch : AppContext -> ProjectRef -> Cmd Msg
fetchMainBranch appContext projectRef =
    ShareApi.projectBranch projectRef BranchRef.main_
        |> HttpApi.toRequest BranchSummary.decode (RemoteData.fromResult >> FetchMainBranchFinished)
        |> HttpApi.perform appContext.api


fetchLatestBranches : AppContext -> ProjectRef -> Cmd Msg
fetchLatestBranches appContext projectRef =
    ShareApi.projectBranches projectRef
        { kind = ShareApi.ProjectBranches
        , searchQuery = Nothing
        , limit = 3
        , cursor = Nothing
        }
        |> HttpApi.toRequest (Decode.field "items" (Decode.list BranchSummary.decode))
            (RemoteData.fromResult >> FetchLatestBranchesFinished)
        |> HttpApi.perform appContext.api


fetchBranchReleaseNotes : AppContext -> ProjectRef -> BranchRef -> Cmd Msg
fetchBranchReleaseNotes appContext projectRef branchRef =
    ShareApi.projectBranchReleaseNotes projectRef branchRef
        |> HttpApi.toRequest
            (Decode.field "doc" (Decode.nullable Doc.decode))
            (FetchReleaseNotesFinished branchRef)
        |> HttpApi.perform appContext.api


publishProjectRelease : AppContext -> ProjectRef -> BranchSummary -> Version -> Cmd Msg
publishProjectRelease appContext projectRef branch version =
    let
        data =
            { branchRef = branch.ref
            , causalHash = branch.causalHash
            , version = version
            }
    in
    ShareApi.createProjectRelease projectRef data
        |> HttpApi.toRequest Release.decode (RemoteData.fromResult >> PublishFinished)
        |> HttpApi.perform appContext.api



-- HELPERS


releaseNotes : Doc -> ReleaseNotes
releaseNotes d =
    { doc = d, interactiveDoc = InteractiveDoc.init, maximized = False }



-- VIEW


viewSheet : WebData (List BranchSummary) -> SelectBranchOpenSheet -> Html Msg
viewSheet suggestions sheet =
    let
        viewSuggestions data =
            [ SearchBranchSheet.viewBranchList "Latest branches" data ]

        suggestions_ =
            { data = suggestions
            , view = viewSuggestions
            }
    in
    sheet.sheet
        |> SearchBranchSheet.view "Select source branch" suggestions_ Nothing
        |> Html.map SearchBranchSheetMsg


viewSelectBranchAnchoredOverlay : WebData (List BranchSummary) -> SelectBranchSheet -> BranchSummary -> AnchoredOverlay Msg
viewSelectBranchAnchoredOverlay suggestions selectBranchSheet branch =
    let
        button caret =
            Button.iconThenLabel ToggleSelectBranchSheet Icon.branch (BranchRef.toString branch.ref)
                |> Button.withIconAfterLabel caret
                |> Button.medium
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay ToggleSelectBranchSheet
    in
    case selectBranchSheet of
        Closed ->
            ao_ (button Icon.caretDown)

        Open sheet ->
            ao_ (button Icon.caretUp)
                |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet suggestions sheet))


viewLoadingModalContent : Html msg
viewLoadingModalContent =
    div [ class "publish-project-release_loading" ]
        [ div [ class "publish-project-release_loading_group" ]
            [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            ]
        , div [ class "publish-project-release_loading_group" ]
            [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Huge |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.view
            , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
            ]
        ]


viewReleaseNotes : BranchRef -> WebData (Maybe ReleaseNotes) -> Html Msg
viewReleaseNotes branchRef releaseNotes_ =
    let
        path =
            "ReleaseNotes Doc on " ++ BranchRef.toString branchRef

        minMaxToggle rn =
            if rn.maximized then
                Button.icon ToggleReleaseNotesSize Icon.minimize

            else
                Button.icon ToggleReleaseNotesSize Icon.maximize

        viewTitle t right =
            header [ class "release-notes-preview_header" ]
                [ div [ class "release-notes-preview_title" ] [ Icon.view Icon.doc, text t ]
                , right
                ]

        releaseNotesLoading =
            [ viewTitle ("Looking up " ++ path) UI.nothing
            , div [ class "release-notes-preview_loading" ]
                [ div
                    [ class "release-notes-preview_loading_items" ]
                    [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.subdued |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.subdued |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.subdued |> Placeholder.view
                    ]
                , div
                    [ class "release-notes-preview_loading_items" ]
                    [ Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.subdued |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Small |> Placeholder.subdued |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Large |> Placeholder.subdued |> Placeholder.view
                    , Placeholder.text |> Placeholder.withLength Placeholder.Medium |> Placeholder.subdued |> Placeholder.view
                    ]
                ]
            ]

        content =
            case releaseNotes_ of
                NotAsked ->
                    releaseNotesLoading

                Loading ->
                    releaseNotesLoading

                Success (Just rn) ->
                    [ viewTitle path (minMaxToggle rn |> Button.small |> Button.view)
                    , Html.map InteractiveDocMsg (InteractiveDoc.view rn.interactiveDoc rn.doc)
                    ]

                Success Nothing ->
                    [ viewTitle ("Couldn't find a " ++ path) UI.nothing
                    ]

                Failure _ ->
                    []
    in
    div [ class "release-notes-preview" ] content


view : Version -> Model -> Html Msg
view currentVersion model =
    let
        nextMajor =
            Version.nextMajor currentVersion

        nextMinor =
            Version.nextMinor currentVersion

        nextPatch =
            Version.nextPatch currentVersion

        versionOptions =
            RadioField.field
                "release-version"
                UpdateVersion
                (NEL.Nonempty
                    (RadioField.option
                        ("Next Major: " ++ Version.toString nextMajor)
                        "For backwards incompatible changes"
                        nextMajor
                    )
                    [ RadioField.option
                        ("Next Minor: " ++ Version.toString nextMinor)
                        "For backwards compatible changes"
                        nextMinor
                    , RadioField.option
                        ("Next Patch: " ++ Version.toString nextPatch)
                        "For backwards compatible bug fixes"
                        nextPatch
                    ]
                )
                model.version

        prepare statusBanner disabled =
            case model.sourceBranch.branch of
                NotAsked ->
                    viewLoadingModalContent

                Loading ->
                    viewLoadingModalContent

                Success sourceBranch ->
                    let
                        releaseNotesMaximized =
                            model.sourceBranch.releaseNotes
                                |> RemoteData.map (Maybe.map .maximized)
                                |> RemoteData.map (Maybe.withDefault False)
                                |> RemoteData.withDefault False
                    in
                    div
                        [ class "publish-project-release_content"
                        , classList
                            [ ( "publish-project-release_content_disabled", disabled )
                            , ( "publish-project-release_release-notes-preview-maximized", releaseNotesMaximized )
                            ]
                        ]
                        [ div [ class "publish-project-release_form-and-release-notes" ]
                            [ section [ class "publish-project-release_form" ]
                                [ section [ class "publish-project-release_select-branch" ]
                                    [ h3 [] [ text "Source Branch" ]
                                    , AnchoredOverlay.view
                                        (viewSelectBranchAnchoredOverlay
                                            model.latestBranches
                                            model.selectBranchSheet
                                            sourceBranch
                                        )
                                    , small [ class "help" ] [ text "The release will be based on the latest change in the selected branch." ]
                                    ]
                                , section [ class "publish-project-release_select-version" ]
                                    [ h3 [] [ text "Version" ]
                                    , RadioField.view versionOptions
                                    ]
                                ]
                            , section [ class "publish-project-release_release-notes" ]
                                [ h3 [] [ text "Release Notes" ]
                                , p []
                                    [ text "Release notes are automatically included if a "
                                    , strong [] [ text "Doc" ]
                                    , text " term named "
                                    , strong [] [ text "ReleaseNotes" ]
                                    , text " exist on the selected source branch."
                                    ]
                                , viewReleaseNotes
                                    model.sourceBranch.branchRef
                                    model.sourceBranch.releaseNotes
                                ]
                            ]
                        , footer [ class "publish-project-release_footer" ]
                            [ statusBanner
                            , Button.button
                                CloseModal
                                "Cancel"
                                |> Button.medium
                                |> Button.subdued
                                |> Button.view
                            , Button.iconThenLabel Publish Icon.rocket "Publish"
                                |> Button.medium
                                |> Button.positive
                                |> Button.view
                            ]
                        ]

                Failure _ ->
                    div [ class "publish-project-release_error" ]
                        [ StatusBanner.bad
                            "Something broke on our end and we couldn't start the publish release process. Please try again."
                        ]

        content =
            case model.publishedRelease of
                NotAsked ->
                    prepare UI.nothing False

                Loading ->
                    prepare (StatusBanner.working "Publishing..") True

                Success _ ->
                    div [ class "publish-project-release_publish-success" ]
                        [ div [ class "publish-project-release_publish-success_emoji" ] [ text "🥳" ]
                        , h1 [] [ text "Congrats!!" ]
                        , p [] [ text "You just released ", strong [] [ text (Version.toString model.version) ] ]
                        ]

                Failure _ ->
                    prepare (StatusBanner.bad "Couldn't publish release, please try again.") False
    in
    content
        |> Modal.content
        |> Modal.modal "publish-project-release-modal" CloseModal
        |> Modal.withHeader "Cut a new release"
        |> Modal.view
