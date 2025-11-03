module UnisonShare.Page.ProjectHistoryPage exposing (..)

import Code.BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Html exposing (Html, div, h3, label, p, span, text)
import Html.Attributes exposing (class)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import String.Extra exposing (pluralize)
import UI
import UI.Card as Card
import UI.DateTime as DateTime
import UI.Icon as Icon exposing (Icon)
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.Placeholder as Placeholder
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Page.ErrorPage as ErrorPage
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectDetails)
import UnisonShare.Project.BranchHistory as BranchHistory exposing (BranchHistory, HistoryEntry)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)



-- MODEL


type alias Model =
    { history : WebData BranchHistory
    }


preInit : Model
preInit =
    { history = NotAsked
    }


init : AppContext -> ProjectDetails -> Maybe BranchRef -> ( Model, Cmd Msg )
init appContext project branchRef =
    let
        branchRef_ : BranchRef
        branchRef_ =
            Maybe.withDefault (Project.defaultBrowsingBranch project) branchRef
    in
    ( { history = Loading }
    , fetchProjectBranchHistory appContext project.ref branchRef_
    )



-- UPDATE


type Msg
    = FetchProjectBranchHistoryFinished BranchRef (WebData BranchHistory)


update : AppContext -> ProjectDetails -> Maybe BranchRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext project branchRef msg model =
    case msg of
        FetchProjectBranchHistoryFinished branchRef_ history ->
            ( { model | history = history }, Cmd.none )



-- EFFECTS


fetchProjectBranchHistory : AppContext -> ProjectRef -> BranchRef -> Cmd Msg
fetchProjectBranchHistory appContext projectRef branchRef =
    let
        mock =
            RemoteData.Success
                { projectRef = projectRef
                , branchRef = branchRef
                , history =
                    [ BranchHistory.Comment
                        { createdAt = DateTime.unsafeFromISO8601 "2025-11-03T13:35:33-05:00"
                        , afterCausalHash = Hash.unsafeFromString "commenthash1"
                        , author = BranchHistory.UnverifiedUser { name = "Simon" }
                        , subject = Just "Just fixed a bunch of stuff"
                        , body = "And it was really cool"
                        }
                    , BranchHistory.Changeset
                        { hash = Hash.unsafeFromString "namespacehash3"
                        , updates =
                            [ { hash = Hash.unsafeFromString "definitionhash1"
                              , fqn = FQN.fromString "List.map"
                              }
                            ]
                        , removes = []
                        , aliases =
                            [ { hash = Hash.unsafeFromString "definitionhash1"
                              , fqn = FQN.fromString "List.map"
                              }
                            ]
                        , renames = []
                        }
                    , BranchHistory.Comment
                        { createdAt = DateTime.unsafeFromISO8601 "2025-11-03T13:35:33-05:00"
                        , afterCausalHash = Hash.unsafeFromString "commenthash1"
                        , author = BranchHistory.UnverifiedUser { name = "Simon" }
                        , subject = Just "Just fixed a bunch of stuff"
                        , body = "And it was really cool"
                        }
                    ]
                }
    in
    Util.delayMsg 500 (FetchProjectBranchHistoryFinished branchRef mock)



{-
   ShareApi.projectBranchHistory projectRef branchRef
       |> HttpApi.toRequest
           BranchHistory.decode
           (RemoteData.fromResult >> FetchProjectBranchHistoryFinished branchRef)
       |> HttpApi.perform appContext.api
-}
-- VIEW


viewLoadingPage : PageLayout msg
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
                [ Card.card
                    [ shape Placeholder.Large
                    , shape Placeholder.Small
                    , shape Placeholder.Medium
                    ]
                    |> Card.asContained
                    |> Card.view
                ]
                |> PageContent.withPageTitle (PageTitle.title "History")
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


viewHistoryEntry_ : String -> Icon msg -> { leftTitle : Html msg, rightTitle : Html msg } -> List (Html msg) -> Html msg
viewHistoryEntry_ className icon { leftTitle, rightTitle } cardContent =
    div [ class ("history-entry " ++ className) ]
        [ div [ class "history-entry_gutter" ]
            [ div [ class "icon-wrapper" ] [ Icon.view icon ]
            ]
        , div [ class "history-entry_title-and-details" ]
            [ div [ class "history-entry_title" ]
                [ leftTitle
                , rightTitle
                ]
            , Card.card cardContent
                |> Card.withClassName "project-history_entry"
                |> Card.asContained
                |> Card.view
            ]
        ]


viewHistoryEntry : AppContext -> HistoryEntry -> Html msg
viewHistoryEntry appContext entry =
    let
        card content =
            Card.card content
                |> Card.withClassName "project-history_entry"
                |> Card.asContained
                |> Card.view
    in
    case entry of
        BranchHistory.Comment comment ->
            let
                createdAt =
                    DateTime.view
                        (DateTime.DistanceFrom appContext.now)
                        appContext.timeZone
                        comment.createdAt

                author =
                    case comment.author of
                        BranchHistory.VerifiedShareUser user ->
                            ProfileSnippet.view (ProfileSnippet.profileSnippet user)

                        BranchHistory.UnverifiedUser { name } ->
                            span [ class "unverified-user" ] [ text name ]
            in
            viewHistoryEntry_
                "history-entry_comment"
                Icon.tag
                { leftTitle = author, rightTitle = createdAt }
                [ comment.subject
                    |> Maybe.map (\s -> h3 [] [ text s ])
                    |> Maybe.withDefault UI.nothing
                , p [] [ text comment.body ]
                ]

        BranchHistory.Changeset changeset ->
            let
                numChanges =
                    (changeset.updates ++ changeset.removes ++ changeset.aliases ++ changeset.renames)
                        |> List.length
                        |> pluralize "change" "changes"

                viewChanges : String -> Icon msg -> String -> List BranchHistory.Change -> Html msg
                viewChanges className icon title changes =
                    if List.isEmpty changes then
                        UI.nothing

                    else
                        div [ class className ]
                            [ div [ class "history-entry_changeset_changes_title" ]
                                [ Icon.view icon, label [] [ text title ] ]
                            , div [ class "history-entry_changeset_changes_list" ]
                                (List.map (.fqn >> FQN.view) changes)
                            ]

                viewCardContent : BranchHistory.ChangesetDetails -> Html msg
                viewCardContent { updates, removes, aliases, renames } =
                    div [ class "history-entry_changeset_changes" ]
                        [ viewChanges "updates" Icon.largePlus "Adds/Updates" updates
                        , viewChanges "removes" Icon.trash "Removes" removes
                        , viewChanges "aliases" Icon.tags "Aliases" aliases
                        , viewChanges "renames" Icon.tag "Renames" renames
                        ]
            in
            viewHistoryEntry_
                "history-entry_changeset"
                Icon.hash
                { leftTitle = Hash.view changeset.hash, rightTitle = text numChanges }
                [ viewCardContent changeset ]


viewPageContent : AppContext -> ProjectDetails -> Maybe BranchRef -> BranchHistory -> PageContent Msg
viewPageContent appContext _ _ history =
    let
        entries =
            if List.isEmpty history.history then
                [ div [] [ text "No entries" ] ]

            else
                List.map (viewHistoryEntry appContext) history.history
    in
    PageContent.oneColumn [ div [ class "history-entries" ] entries ]
        |> PageContent.withPageTitle (PageTitle.title "History")


view : AppContext -> ProjectDetails -> Maybe BranchRef -> Model -> ( PageLayout Msg, Maybe (Html Msg) )
view appContext project branchRef model =
    case model.history of
        NotAsked ->
            ( viewLoadingPage, Nothing )

        Loading ->
            ( viewLoadingPage, Nothing )

        Success history ->
            ( PageLayout.centeredNarrowLayout
                (viewPageContent appContext project branchRef history)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
            , Nothing
            )

        Failure e ->
            ( ErrorPage.view appContext.session e "history" "project-history-page"
            , Nothing
            )
