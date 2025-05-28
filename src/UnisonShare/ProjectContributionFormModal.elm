module UnisonShare.ProjectContributionFormModal exposing
    ( FormAction(..)
    , Model
    , Msg
    , OutMsg(..)
    , init
    , update
    , view
    )

import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html, aside, div, p, section, span, strong, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra as StringE
import Task exposing (Task)
import UI
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.StatusBanner as StatusBanner
import UI.StatusMessage as StatusMessage
import UnisonShare.Account exposing (AccountSummary)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.Contribution as Contribution exposing (ContributionSummary)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Link as Link
import UnisonShare.Paginated as Paginated
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.SearchBranchSheet as SearchBranchSheet



-- MODEL


type alias SelectBranchOpenSheet =
    { sheet : SearchBranchSheet.Model }


type SelectBranchSheet
    = OpenForSource SelectBranchOpenSheet
    | OpenForTarget SelectBranchOpenSheet
    | Closed


type alias RecentBranches =
    { ownContributorBranches : List BranchSummary
    , projectBranches : List BranchSummary
    }


type SourceBranchValidity
    = Missing
    | SameAsTarget
    | ValidSourceBranch


type Validity
    = NotChecked
    | Valid { sourceBranchRef : BranchRef }
    | Invalid
        { needsTitle : Bool
        , sourceBranch : SourceBranchValidity
        }


type alias Form =
    { title : String
    , description : String
    , sourceBranchRef : Maybe BranchRef
    , targetBranchRef : BranchRef
    , selectBranchSheet : SelectBranchSheet
    , save : WebData ContributionSummary
    , recentBranches : RecentBranches
    , validity : Validity
    }


type FormAction
    = Edit ContributionSummary
    | Create


type alias Model =
    { form : WebData Form
    , action : FormAction
    }


init : AppContext -> AccountSummary -> ProjectRef -> FormAction -> ( Model, Cmd Msg )
init appContext currentUser projectRef action =
    ( { form = Loading, action = action }
    , Task.attempt FetchRecentBranchesFinished
        (fetchRecentBranches appContext currentUser projectRef)
    )



-- UPDATE


type Msg
    = CloseModal
    | FetchRecentBranchesFinished (HttpResult RecentBranches)
    | UpdateSubmissionTitle String
    | UpdateSubmissionDescription String
    | ToggleSelectSourceBranchSheet
    | ToggleSelectTargetBranchSheet
    | SearchBranchSheetMsg SearchBranchSheet.Msg
    | SaveContribution
    | SaveContributionFinished (WebData ContributionSummary)
    | SuccessfullySaved ContributionSummary


type OutMsg
    = None
    | RequestToCloseModal
    | Saved ContributionSummary


update : AppContext -> ProjectDetails -> AccountSummary -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext project account msg model =
    case ( msg, model.form, model.action ) of
        ( CloseModal, _, _ ) ->
            ( model, Cmd.none, RequestToCloseModal )

        ( FetchRecentBranchesFinished recentBranches, _, Edit contrib ) ->
            case recentBranches of
                Ok rb ->
                    let
                        form =
                            { title = contrib.title
                            , description = Maybe.withDefault "" contrib.description
                            , sourceBranchRef = Just contrib.sourceBranchRef
                            , targetBranchRef = BranchRef.main_
                            , selectBranchSheet = Closed
                            , save = NotAsked
                            , recentBranches = rb
                            , validity = NotChecked
                            }
                    in
                    ( { model | form = Success form }, Cmd.none, None )

                Err e ->
                    ( { model | form = Failure e }, Cmd.none, None )

        ( FetchRecentBranchesFinished recentBranches, _, Create ) ->
            case recentBranches of
                Ok rb ->
                    let
                        sourceBranchRef =
                            rb.ownContributorBranches
                                |> List.head
                                |> Maybe.map .ref

                        form =
                            { title = ""
                            , description = ""
                            , sourceBranchRef = sourceBranchRef
                            , targetBranchRef = BranchRef.main_
                            , selectBranchSheet = Closed
                            , save = NotAsked
                            , recentBranches = rb
                            , validity = NotChecked
                            }
                    in
                    ( { model | form = Success form }, Cmd.none, None )

                Err e ->
                    ( { model | form = Failure e }, Cmd.none, None )

        ( ToggleSelectSourceBranchSheet, Success form, _ ) ->
            let
                form_ =
                    let
                        filter =
                            if Project.canContribute project then
                                ShareApi.AllBranches (Just account.handle)

                            else
                                ShareApi.ContributorBranches (Just account.handle)

                        selectBranchSheet =
                            case form.selectBranchSheet of
                                OpenForSource _ ->
                                    Closed

                                OpenForTarget _ ->
                                    Closed

                                Closed ->
                                    OpenForSource { sheet = SearchBranchSheet.init filter }
                    in
                    { form | selectBranchSheet = selectBranchSheet }
            in
            ( { model | form = Success form_ }, Cmd.none, None )

        ( ToggleSelectTargetBranchSheet, Success form, _ ) ->
            let
                form_ =
                    let
                        selectBranchSheet =
                            case form.selectBranchSheet of
                                OpenForSource _ ->
                                    Closed

                                OpenForTarget _ ->
                                    Closed

                                Closed ->
                                    OpenForTarget { sheet = SearchBranchSheet.init (ShareApi.AllBranches Nothing) }
                    in
                    { form | selectBranchSheet = selectBranchSheet }
            in
            ( { model | form = Success form_ }, Cmd.none, None )

        ( SearchBranchSheetMsg sbsMsg, Success form, _ ) ->
            case form.selectBranchSheet of
                OpenForSource s ->
                    let
                        ( sheet_, cmd, sbsOut ) =
                            SearchBranchSheet.update appContext project.ref sbsMsg s.sheet

                        newForm =
                            case ( sbsOut, RemoteData.map .selectBranchSheet model.form ) of
                                ( SearchBranchSheet.SelectBranchRequest branch, Success (OpenForSource _) ) ->
                                    { form | sourceBranchRef = Just branch.ref, selectBranchSheet = Closed }

                                ( SearchBranchSheet.SelectBranchRequest branch, Success (OpenForTarget _) ) ->
                                    { form | targetBranchRef = branch.ref, selectBranchSheet = Closed }

                                _ ->
                                    { form | selectBranchSheet = OpenForSource { s | sheet = sheet_ } }
                    in
                    ( { model | form = Success newForm }
                    , Cmd.map SearchBranchSheetMsg cmd
                    , None
                    )

                OpenForTarget t ->
                    let
                        ( sheet_, cmd, sbsOut ) =
                            SearchBranchSheet.update appContext project.ref sbsMsg t.sheet

                        newForm =
                            case sbsOut of
                                SearchBranchSheet.NoOutMsg ->
                                    { form | selectBranchSheet = OpenForTarget { t | sheet = sheet_ } }

                                SearchBranchSheet.SelectBranchRequest branch ->
                                    { form | targetBranchRef = branch.ref, selectBranchSheet = Closed }
                    in
                    ( { model | form = Success newForm }
                    , Cmd.map SearchBranchSheetMsg cmd
                    , None
                    )

                Closed ->
                    ( model, Cmd.none, None )

        ( UpdateSubmissionTitle t, Success form, _ ) ->
            let
                newForm =
                    { form | title = t }
            in
            ( { model | form = Success newForm }, Cmd.none, None )

        ( UpdateSubmissionDescription d, Success form, _ ) ->
            let
                newForm =
                    { form | description = d }
            in
            ( { model | form = Success newForm }, Cmd.none, None )

        ( SaveContribution, Success form, Create ) ->
            let
                validity =
                    case ( form.sourceBranchRef, form.title ) of
                        ( Nothing, "" ) ->
                            Invalid { needsTitle = True, sourceBranch = Missing }

                        ( Nothing, _ ) ->
                            Invalid { needsTitle = False, sourceBranch = Missing }

                        ( Just sbr, "" ) ->
                            if BranchRef.equals sbr form.targetBranchRef then
                                Invalid { needsTitle = True, sourceBranch = SameAsTarget }

                            else
                                Invalid { needsTitle = True, sourceBranch = ValidSourceBranch }

                        ( Just sbr, _ ) ->
                            if BranchRef.equals sbr form.targetBranchRef then
                                Invalid { needsTitle = False, sourceBranch = SameAsTarget }

                            else
                                Valid { sourceBranchRef = sbr }
            in
            case validity of
                Valid { sourceBranchRef } ->
                    if not (String.isEmpty form.title) then
                        let
                            newForm =
                                { form | save = Loading }

                            newContribution =
                                { title = form.title
                                , description = StringE.nonEmpty form.description
                                , status = ContributionStatus.InReview
                                , sourceBranchRef = sourceBranchRef
                                , targetBranchRef = form.targetBranchRef
                                }
                        in
                        ( { model | form = Success newForm }
                        , createProjectContribution appContext project.ref newContribution
                        , None
                        )

                    else
                        ( model, Cmd.none, None )

                _ ->
                    ( { model | form = Success { form | validity = validity } }, Cmd.none, None )

        ( SaveContribution, Success form, Edit c ) ->
            case form.sourceBranchRef of
                Just sourceBranchRef ->
                    let
                        newForm =
                            { form | save = Loading }

                        updatedContribution =
                            { title = form.title
                            , description = StringE.nonEmpty form.description
                            , status = c.status
                            , sourceBranchRef = sourceBranchRef
                            , targetBranchRef = form.targetBranchRef
                            }
                    in
                    ( { model | form = Success newForm }
                    , updateProjectContribution appContext
                        project.ref
                        c.ref
                        updatedContribution
                    , None
                    )

                _ ->
                    ( model, Cmd.none, None )

        ( SaveContributionFinished contribution, Success form, _ ) ->
            let
                cmd =
                    case contribution of
                        Success c ->
                            Util.delayMsg 1500 (SuccessfullySaved c)

                        _ ->
                            Cmd.none
            in
            ( { model | form = Success { form | save = contribution } }, cmd, None )

        ( SuccessfullySaved contrib, _, _ ) ->
            ( model, Cmd.none, Saved contrib )

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


fetchRecentBranches : AppContext -> AccountSummary -> ProjectRef -> Task Http.Error RecentBranches
fetchRecentBranches appContext currentUser projectRef =
    let
        contributorBranchesParams =
            { kind = ShareApi.ContributorBranches (Just currentUser.handle)
            , searchQuery = Nothing
            , limit = 3
            , cursor = Paginated.NoPageCursor
            }

        projectBranchesParams =
            { kind = ShareApi.ProjectBranches
            , searchQuery = Nothing
            , limit = 3
            , cursor = Paginated.NoPageCursor
            }
    in
    Task.map2
        RecentBranches
        (fetchBranches appContext projectRef contributorBranchesParams)
        (fetchBranches appContext projectRef projectBranchesParams)


fetchBranches :
    AppContext
    -> ProjectRef
    -> ShareApi.ProjectBranchesParams
    -> Task Http.Error (List BranchSummary)
fetchBranches appContext projectRef params =
    HttpApi.toTask appContext.api.url
        (Decode.field "items" (Decode.list BranchSummary.decode))
        (ShareApi.projectBranches projectRef params)


createProjectContribution : AppContext -> ProjectRef -> ShareApi.NewProjectContribution -> Cmd Msg
createProjectContribution appContext projectRef newContribution =
    ShareApi.createProjectContribution projectRef newContribution
        |> HttpApi.toRequest
            Contribution.decodeSummary
            (RemoteData.fromResult >> SaveContributionFinished)
        |> HttpApi.perform appContext.api


updateProjectContribution :
    AppContext
    -> ProjectRef
    -> ContributionRef
    ->
        { title : String
        , description : Maybe String
        , status : ContributionStatus
        , sourceBranchRef : BranchRef
        , targetBranchRef : BranchRef
        }
    -> Cmd Msg
updateProjectContribution appContext projectRef contribRef updates =
    ShareApi.updateProjectContribution projectRef contribRef (ShareApi.ProjectContributionUpdate updates)
        |> HttpApi.toRequest
            Contribution.decodeSummary
            (RemoteData.fromResult >> SaveContributionFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewSheet : SelectBranchOpenSheet -> List BranchSummary -> Html Msg
viewSheet sheet suggestions =
    let
        viewSuggestions brs =
            case brs of
                [] ->
                    []

                _ ->
                    [ SearchBranchSheet.viewBranchList "Most recent branches" brs ]

        suggestions_ =
            { data = Success suggestions
            , view = viewSuggestions
            }
    in
    sheet.sheet
        |> SearchBranchSheet.view "Select Branch" suggestions_ Nothing
        |> Html.map SearchBranchSheetMsg


viewSelectSourceBranchAnchoredOverlay : SelectBranchSheet -> Maybe BranchRef -> List BranchSummary -> AnchoredOverlay Msg
viewSelectSourceBranchAnchoredOverlay selectBranchSheet branchRef ownContributorBranches =
    let
        button caret =
            Button.iconThenLabel ToggleSelectSourceBranchSheet
                Icon.branch
                (MaybeE.unwrap "Select Branch" BranchRef.toString branchRef)
                |> Button.withIconAfterLabel caret
                |> Button.small
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay ToggleSelectSourceBranchSheet
    in
    case selectBranchSheet of
        Closed ->
            ao_ (button Icon.caretDown)

        OpenForSource sheet ->
            ao_ (button Icon.caretUp)
                |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet sheet ownContributorBranches))
                |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomLeft

        OpenForTarget _ ->
            ao_ (button Icon.caretDown)


viewSelectTargetBranchAnchoredOverlay : SelectBranchSheet -> BranchRef -> List BranchSummary -> AnchoredOverlay Msg
viewSelectTargetBranchAnchoredOverlay selectBranchSheet branchRef projectBranches =
    let
        button caret =
            Button.iconThenLabel ToggleSelectTargetBranchSheet
                Icon.branch
                (BranchRef.toString branchRef)
                |> Button.withIconAfterLabel caret
                |> Button.small
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay ToggleSelectSourceBranchSheet
    in
    case selectBranchSheet of
        Closed ->
            ao_ (button Icon.caretDown)

        OpenForSource _ ->
            ao_ (button Icon.caretDown)

        OpenForTarget sheet ->
            ao_ (button Icon.caretUp)
                |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet sheet projectBranches))
                |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomLeft


view : ProjectRef -> String -> Model -> Html Msg
view projectRef title model =
    case model.form of
        Success form ->
            let
                titleValidity tf =
                    case form.validity of
                        Invalid { needsTitle } ->
                            if needsTitle then
                                TextField.markAsInvalid tf

                            else
                                tf

                        _ ->
                            tf

                sourceBranchInvalid =
                    case form.validity of
                        Invalid { sourceBranch } ->
                            case sourceBranch of
                                Missing ->
                                    span [ class "source-branch_invalid" ]
                                        [ Icon.view Icon.arrowLeftUp
                                        , text "Please provide a source branch."
                                        ]

                                SameAsTarget ->
                                    span [ class "source-branch_invalid" ]
                                        [ Icon.view Icon.arrowLeftUp
                                        , text "Please provide different source and target branches."
                                        ]

                                _ ->
                                    UI.nothing

                        _ ->
                            UI.nothing

                viewForm =
                    div [ class "form" ]
                        [ section [ class "branches" ]
                            [ text "Request to merge"
                            , span [ class "source-branch" ]
                                [ AnchoredOverlay.view
                                    (viewSelectSourceBranchAnchoredOverlay
                                        form.selectBranchSheet
                                        form.sourceBranchRef
                                        form.recentBranches.ownContributorBranches
                                    )
                                , sourceBranchInvalid
                                ]
                            , text "into"
                            , AnchoredOverlay.view
                                (viewSelectTargetBranchAnchoredOverlay
                                    form.selectBranchSheet
                                    form.targetBranchRef
                                    form.recentBranches.projectBranches
                                )
                            ]
                        , section [ class "fields" ]
                            [ TextField.field UpdateSubmissionTitle "Title" form.title
                                |> TextField.withAutofocus
                                |> TextField.withHelpText "Required. Ex. 'Add List.map'"
                                |> titleValidity
                                |> TextField.view
                            , TextField.field UpdateSubmissionDescription "Description" form.description
                                |> TextField.withHelpText "Provide a detailed description of your contribution; what it solves and how."
                                |> TextField.withRows 8
                                |> TextField.view
                            ]
                        , aside [ class "small-print" ]
                            [ span [ class "info-icon" ] [ Icon.view Icon.info ]
                            , p []
                                [ text "By submitting this contribution, you agree to license it using "
                                , strong [] [ text (ProjectRef.toString projectRef) ]
                                , text "'s existing license."
                                , text " Please read the "
                                , Link.view "Terms of Service" Link.termsOfService
                                , text " for more detail."
                                ]
                            ]
                        ]

                ( content, leftSideFooter, dimOverlay ) =
                    case form.save of
                        NotAsked ->
                            ( viewForm, [], False )

                        Loading ->
                            ( viewForm, [ StatusBanner.working "Saving.." ], True )

                        Success contrib ->
                            let
                                status =
                                    case model.action of
                                        Create ->
                                            StatusMessage.good
                                                ("Successfully created contribution " ++ ContributionRef.toString contrib.ref)
                                                []

                                        Edit _ ->
                                            StatusMessage.good
                                                ("Successfully updated contribution " ++ ContributionRef.toString contrib.ref)
                                                []
                            in
                            ( div [ class "saved" ] [ StatusMessage.view status ], [], False )

                        Failure _ ->
                            ( viewForm, [ StatusBanner.bad "Couldn't save, please try again." ], False )
            in
            content
                |> Modal.content
                |> Modal.modal "project-contribution-form-modal" CloseModal
                |> Modal.withActionsIf
                    [ Button.button CloseModal "Cancel"
                        |> Button.medium
                        |> Button.subdued
                    , Button.iconThenLabel SaveContribution Icon.rocket title
                        |> Button.medium
                        |> Button.positive
                    ]
                    (not (RemoteData.isSuccess form.save))
                |> Modal.withLeftSideFooter leftSideFooter
                |> Modal.withDimOverlay dimOverlay
                |> Modal.withHeaderIf "Save contribution" (not (RemoteData.isSuccess form.save))
                |> Modal.view

        _ ->
            Modal.content UI.nothing
                |> Modal.modal "submit-contribution-modal" CloseModal
                |> Modal.withHeader "Save contribution"
                |> Modal.view
