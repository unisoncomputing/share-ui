module UnisonShare.SwitchBranch exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Html exposing (Html)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Maybe.Extra as MaybeE
import RemoteData exposing (WebData)
import UI.AnchoredOverlay as AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Icon as Icon
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchSummary as BranchSummary exposing (BranchSummary)
import UnisonShare.Link as Link
import UnisonShare.Paginated as Paginated
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.SearchBranchSheet as SearchBranchSheet
import UnisonShare.Session as Session



-- MODEL


type alias RecentBranches =
    { -- Its a Maybe, because we might not be signed in
      ownContributorBranches : Maybe (WebData (List BranchSummary))
    , projectBranches : WebData (List BranchSummary)
    }


type alias Sheet =
    { sheet : SearchBranchSheet.Model
    , recentBranches : RecentBranches
    }


type Model
    = Closed
    | Open Sheet


init : Model
init =
    Closed



-- UPDATE


type Msg
    = ToggleSheet
    | CloseSheet
    | FetchOwnContributorBranchesFinished (HttpResult (List BranchSummary))
    | FetchProjectBranchesFinished (HttpResult (List BranchSummary))
    | SearchBranchSheetMsg SearchBranchSheet.Msg


type OutMsg
    = None
    | SwitchToBranchRequest BranchRef


update : AppContext -> ProjectRef -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext projectRef msg model =
    case ( msg, model ) of
        ( ToggleSheet, Closed ) ->
            let
                ( contributorBranchesCmd, ownContributorBranches ) =
                    case appContext.session of
                        Session.Anonymous ->
                            ( Cmd.none, Nothing )

                        Session.SignedIn { handle } ->
                            ( fetchBranches FetchOwnContributorBranchesFinished
                                appContext
                                projectRef
                                { kind = ShareApi.ContributorBranches (Just handle)
                                , searchQuery = Nothing
                                , limit = 3
                                , cursor = Paginated.NoPageCursor
                                }
                            , Just RemoteData.Loading
                            )

                sheet =
                    { sheet = SearchBranchSheet.init (ShareApi.AllBranches Nothing)
                    , recentBranches =
                        { ownContributorBranches = ownContributorBranches
                        , projectBranches = RemoteData.Loading
                        }
                    }

                projectBranchesParams =
                    { kind = ShareApi.ProjectBranches
                    , searchQuery = Nothing
                    , limit = 3
                    , cursor = Paginated.NoPageCursor
                    }
            in
            ( Open sheet
            , Cmd.batch
                [ contributorBranchesCmd
                , fetchBranches FetchProjectBranchesFinished
                    appContext
                    projectRef
                    projectBranchesParams
                ]
            , None
            )

        ( ToggleSheet, Open _ ) ->
            ( Closed, Cmd.none, None )

        ( CloseSheet, _ ) ->
            ( Closed, Cmd.none, None )

        ( FetchOwnContributorBranchesFinished branches, Open s ) ->
            let
                rb =
                    s.recentBranches

                sheet =
                    { s | recentBranches = { rb | ownContributorBranches = Just (RemoteData.fromResult branches) } }
            in
            ( Open sheet, Cmd.none, None )

        ( FetchProjectBranchesFinished branches, Open s ) ->
            let
                rb =
                    s.recentBranches

                sheet =
                    { s | recentBranches = { rb | projectBranches = RemoteData.fromResult branches } }
            in
            ( Open sheet, Cmd.none, None )

        ( SearchBranchSheetMsg sbsMsg, Open sheet ) ->
            let
                ( newSheet, cmd, sbsOut ) =
                    SearchBranchSheet.update appContext projectRef sbsMsg sheet.sheet
            in
            case sbsOut of
                SearchBranchSheet.NoOutMsg ->
                    ( Open { sheet | sheet = newSheet }, Cmd.map SearchBranchSheetMsg cmd, None )

                SearchBranchSheet.SelectBranchRequest br ->
                    ( Closed, Cmd.map SearchBranchSheetMsg cmd, SwitchToBranchRequest br.ref )

        _ ->
            ( model, Cmd.none, None )



-- EFFECTS


fetchBranches :
    (HttpResult (List BranchSummary) -> Msg)
    -> AppContext
    -> ProjectRef
    -> ShareApi.ProjectBranchesParams
    -> Cmd Msg
fetchBranches doneMsg appContext projectRef params =
    ShareApi.projectBranches projectRef params
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list BranchSummary.decode))
            doneMsg
        |> HttpApi.perform appContext.api



-- VIEW


type alias SuggestionsData =
    { ownContributorBranches : List BranchSummary
    , projectBranches : List BranchSummary
    }


viewSuggestions : SuggestionsData -> List (Html SearchBranchSheet.Msg)
viewSuggestions data =
    let
        ownContributorBranches =
            if List.isEmpty data.ownContributorBranches then
                Nothing

            else
                Just (SearchBranchSheet.viewBranchList "My contributor branches" data.ownContributorBranches)

        projectBranches =
            if List.isEmpty data.projectBranches then
                Nothing

            else
                Just (SearchBranchSheet.viewBranchList "Most recent project branches" data.projectBranches)
    in
    MaybeE.values [ ownContributorBranches, projectBranches ]


viewSheet : ProjectRef -> Sheet -> Html Msg
viewSheet projectRef sheet =
    let
        recentBranches =
            RemoteData.map2
                (\ownContributorBranches projectBranches ->
                    { ownContributorBranches = ownContributorBranches
                    , projectBranches = projectBranches
                    }
                )
                -- If we don't have a `ownContributorBranches`, its because we're not signed in.
                -- In that case, we just want to use an empty list.
                (Maybe.withDefault (RemoteData.Success [])
                    sheet.recentBranches.ownContributorBranches
                )
                sheet.recentBranches.projectBranches

        suggestions =
            { data = recentBranches
            , view = viewSuggestions
            }
    in
    Html.map SearchBranchSheetMsg
        (SearchBranchSheet.view
            "Switch Branch"
            suggestions
            (Just (Link.view "View all branches" (Link.projectBranches projectRef)))
            sheet.sheet
        )


toAnchoredOverlay : ProjectRef -> BranchRef -> Model -> AnchoredOverlay Msg
toAnchoredOverlay projectRef branchRef model =
    let
        button caret =
            Button.iconThenLabel ToggleSheet Icon.branch (BranchRef.toString branchRef)
                |> Button.withIconAfterLabel caret
                |> Button.small
                |> Button.stopPropagation
                |> Button.view

        ao_ =
            AnchoredOverlay.anchoredOverlay CloseSheet
    in
    case model of
        Closed ->
            ao_ (button Icon.caretDown)

        Open sheet ->
            ao_ (button Icon.caretUp)
                |> AnchoredOverlay.withSheetPosition AnchoredOverlay.BottomLeft
                |> AnchoredOverlay.withSheet (AnchoredOverlay.sheet (viewSheet projectRef sheet))
