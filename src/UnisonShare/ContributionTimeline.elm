module UnisonShare.ContributionTimeline exposing (..)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import Task
import UI.Icon as Icon
import UI.Placeholder as Placeholder
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Contribution.ContributionEvent as ContributionEvent exposing (ContributionEvent(..))
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus exposing (ContributionStatus(..))
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Timeline.CommentEvent as CommentEvent exposing (CommentDetails)
import UnisonShare.Timeline.CommentId as CommentId exposing (CommentId)
import UnisonShare.Timeline.StatusChangeEvent as StatusChangeEvent


type alias ContributionTimeline =
    List ContributionEvent


type alias ModifyCommentRequests =
    Dict
        -- This is a CommentId
        String
        { original : CommentDetails
        , request : CommentEvent.ModifyCommentRequest
        }


type alias Model =
    { timeline : WebData ContributionTimeline
    , modifyCommentRequests : ModifyCommentRequests
    , newComment : CommentEvent.NewComment
    }


init : AppContext -> ProjectRef -> ContributionRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    ( { timeline = Loading
      , modifyCommentRequests = Dict.empty
      , newComment = CommentEvent.WritingComment ""
      }
    , fetchContributionTimeline appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = NoOp
    | FetchContributionTimelineFinished (WebData ContributionTimeline)
    | UpdateNewComment String
    | PostNewComment
    | PostNewCommentFinished String (HttpResult CommentDetails)
    | ResetNewComment
    | ShowDeleteCommentConfirmation CommentId
    | CancelDeleteComment CommentId
    | DeleteComment CommentId
    | DeleteCommentFinished CommentId (WebData ())
    | EditComment CommentId
    | UpdateEditingComment CommentId String
    | SaveEditComment CommentId String
    | SaveEditCommentFinished CommentId String (HttpResult CommentDetails)
    | CancelEditComment CommentId


update : AppContext -> ProjectRef -> ContributionRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef contribRef msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchContributionTimelineFinished timeline ->
            ( { model | timeline = timeline }, Cmd.none )

        UpdateNewComment text ->
            ( { model | newComment = CommentEvent.WritingComment text }, Cmd.none )

        PostNewComment ->
            case model.newComment of
                CommentEvent.WritingComment text ->
                    if not (String.isEmpty text) then
                        ( { model | newComment = CommentEvent.PostingComment text }
                        , postContributionComment appContext projectRef contribRef text
                        )

                    else
                        ( model, Cmd.none )

                CommentEvent.CommentPostingFailure text _ ->
                    if not (String.isEmpty text) then
                        ( { model | newComment = CommentEvent.PostingComment text }
                        , postContributionComment appContext projectRef contribRef text
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PostNewCommentFinished _ (Ok comment) ->
            let
                commentEvent =
                    ContributionEvent.Comment comment

                timeline =
                    model.timeline
                        |> RemoteData.map (\l -> l ++ [ commentEvent ])
            in
            ( { model | timeline = timeline, newComment = CommentEvent.CommentPostingSuccess comment }
            , Util.delayMsg 2500 ResetNewComment
            )

        PostNewCommentFinished text (Err e) ->
            ( { model | newComment = CommentEvent.CommentPostingFailure text e }
            , Cmd.none
            )

        ResetNewComment ->
            case model.newComment of
                CommentEvent.CommentPostingSuccess _ ->
                    ( { model | newComment = CommentEvent.WritingComment "" }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        EditComment commentId ->
            let
                f evt acc =
                    case evt of
                        Comment details ->
                            if CommentId.equals commentId details.id then
                                Just details

                            else
                                acc

                        _ ->
                            acc

                original =
                    model.timeline
                        |> RemoteData.map (List.foldl f Nothing)
                        |> RemoteData.withDefault Nothing
            in
            case original of
                Just orig ->
                    let
                        modifyCommentRequests =
                            Dict.insert
                                (CommentId.toString orig.id)
                                { original = orig
                                , request = CommentEvent.Editing orig.content
                                }
                                model.modifyCommentRequests
                    in
                    ( { model | modifyCommentRequests = modifyCommentRequests }
                    , Dom.focus ("edit_" ++ CommentId.toString orig.id) |> Task.attempt (always NoOp)
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateEditingComment id edit ->
            let
                modifyCommentRequests =
                    Dict.update
                        (CommentId.toString id)
                        (Maybe.map (\r -> { r | request = CommentEvent.Editing edit }))
                        model.modifyCommentRequests
            in
            ( { model | modifyCommentRequests = modifyCommentRequests }
            , Cmd.none
            )

        SaveEditComment id edit ->
            case Dict.get (CommentId.toString id) model.modifyCommentRequests of
                Just { original } ->
                    let
                        modifyCommentRequests =
                            Dict.update
                                (CommentId.toString id)
                                (Maybe.map (\r -> { r | request = CommentEvent.SavingEdit edit }))
                                model.modifyCommentRequests
                    in
                    ( { model | modifyCommentRequests = modifyCommentRequests }
                    , updateContributionComment appContext projectRef contribRef id original.revision edit
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveEditCommentFinished id _ (Ok comment) ->
            let
                replaceComment evt =
                    case evt of
                        ContributionEvent.Comment c ->
                            if CommentId.equals c.id id then
                                ContributionEvent.Comment comment

                            else
                                evt

                        _ ->
                            evt

                timeline =
                    model.timeline
                        |> RemoteData.map (List.map replaceComment)

                modifyCommentRequests =
                    Dict.update
                        (CommentId.toString id)
                        (Maybe.map (\r -> { r | request = CommentEvent.EditSaved }))
                        model.modifyCommentRequests
            in
            ( { model | timeline = timeline, modifyCommentRequests = modifyCommentRequests }
            , Cmd.none
            )

        SaveEditCommentFinished id _ (Err e) ->
            let
                modifyCommentRequests =
                    Dict.update
                        (CommentId.toString id)
                        (Maybe.map (\r -> { r | request = CommentEvent.EditFailure e }))
                        model.modifyCommentRequests
            in
            ( { model | modifyCommentRequests = modifyCommentRequests }
            , Cmd.none
            )

        CancelEditComment id ->
            let
                modifyCommentRequests =
                    Dict.remove (CommentId.toString id) model.modifyCommentRequests
            in
            ( { model | modifyCommentRequests = modifyCommentRequests }
            , Cmd.none
            )

        ShowDeleteCommentConfirmation commentId ->
            let
                f evt acc =
                    case evt of
                        Comment details ->
                            if CommentId.equals commentId details.id then
                                Just details

                            else
                                acc

                        _ ->
                            Nothing

                original =
                    model.timeline
                        |> RemoteData.map (List.foldl f Nothing)
                        |> RemoteData.withDefault Nothing
            in
            case original of
                Just orig ->
                    let
                        modifyCommentRequests =
                            Dict.insert
                                (CommentId.toString orig.id)
                                { original = orig
                                , request = CommentEvent.ConfirmDelete
                                }
                                model.modifyCommentRequests
                    in
                    ( { model | modifyCommentRequests = modifyCommentRequests }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        CancelDeleteComment id ->
            let
                modifyCommentRequests =
                    Dict.remove (CommentId.toString id) model.modifyCommentRequests
            in
            ( { model | modifyCommentRequests = modifyCommentRequests }
            , Cmd.none
            )

        DeleteComment commentId ->
            let
                removedDetails c =
                    { id = commentId
                    , timestamp = c.timestamp
                    , deletedAt = appContext.now
                    }

                eventToRemoved idToRemove evt ( evts, removed ) =
                    case evt of
                        Comment d ->
                            if CommentId.equals d.id idToRemove then
                                ( CommentRemoved (removedDetails d) :: evts, Just d )

                            else
                                ( evt :: evts, removed )

                        _ ->
                            ( evt :: evts, removed )

                ( timeline, removedComment ) =
                    model.timeline
                        |> RemoteData.map (List.foldr (eventToRemoved commentId) ( [], Nothing ))
                        |> RemoteData.unwrap ( model.timeline, Nothing ) (\( t, r ) -> ( Success t, r ))
            in
            case removedComment of
                Just c ->
                    let
                        modifyCommentRequests =
                            Dict.insert
                                (CommentId.toString commentId)
                                { original = c, request = CommentEvent.Deleting }
                                model.modifyCommentRequests
                    in
                    ( { model
                        | timeline = timeline
                        , modifyCommentRequests = modifyCommentRequests
                      }
                    , deleteContributionComment
                        appContext
                        projectRef
                        contribRef
                        commentId
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteCommentFinished commentId res ->
            let
                timeline =
                    case ( res, Dict.get (CommentId.toString commentId) model.modifyCommentRequests ) of
                        ( Failure _, Just c ) ->
                            let
                                eventToRemoved evt =
                                    case evt of
                                        CommentRemoved d ->
                                            if CommentId.equals d.id commentId then
                                                Comment c.original

                                            else
                                                evt

                                        _ ->
                                            evt
                            in
                            model.timeline
                                |> RemoteData.map (List.map eventToRemoved)

                        _ ->
                            model.timeline

                request r =
                    case res of
                        Success _ ->
                            CommentEvent.Deleted

                        Failure err ->
                            CommentEvent.DeleteFailure err

                        _ ->
                            r.request

                modifyCommentRequests =
                    Dict.update
                        (CommentId.toString commentId)
                        (Maybe.map (\r -> { r | request = request r }))
                        model.modifyCommentRequests
            in
            ( { model | timeline = timeline, modifyCommentRequests = modifyCommentRequests }, Cmd.none )


addEvent : Model -> ContributionEvent -> Model
addEvent model event =
    let
        timeline =
            model.timeline
                |> RemoteData.map (\tl -> tl ++ [ event ])
    in
    { model | timeline = timeline }


isUpdatable : Model -> Bool
isUpdatable model =
    RemoteData.isSuccess model.timeline



-- EFFECTS


fetchContributionTimeline : AppContext -> ProjectRef -> ContributionRef -> Cmd Msg
fetchContributionTimeline appContext projectRef contributionRef =
    ShareApi.projectContributionTimeline projectRef contributionRef
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list ContributionEvent.decode))
            (RemoteData.fromResult >> FetchContributionTimelineFinished)
        |> HttpApi.perform appContext.api


postContributionComment : AppContext -> ProjectRef -> ContributionRef -> String -> Cmd Msg
postContributionComment appContext projectRef contributionRef text =
    ShareApi.createProjectContributionComment projectRef contributionRef text
        |> HttpApi.toRequest CommentEvent.decodeCommentDetails (PostNewCommentFinished text)
        |> HttpApi.perform appContext.api


updateContributionComment : AppContext -> ProjectRef -> ContributionRef -> CommentId -> Int -> String -> Cmd Msg
updateContributionComment appContext projectRef contributionRef commentId originalRevision text =
    ShareApi.updateProjectContributionComment projectRef contributionRef commentId originalRevision text
        |> HttpApi.toRequest (Decode.field "comment" CommentEvent.decodeCommentDetails) (SaveEditCommentFinished commentId text)
        |> HttpApi.perform appContext.api


deleteContributionComment : AppContext -> ProjectRef -> ContributionRef -> CommentId -> Cmd Msg
deleteContributionComment appContext projectRef contributionRef commentId =
    ShareApi.deleteProjectContributionComment projectRef contributionRef commentId
        |> HttpApi.toRequestWithEmptyResponse
            (RemoteData.fromResult >> DeleteCommentFinished commentId)
        |> HttpApi.perform appContext.api



-- VIEW


viewStatusChangeEvent : DateTimeContext a -> ContributionEvent.StatusChangeDetails -> Html Msg
viewStatusChangeEvent dtContext ({ newStatus, oldStatus } as details) =
    case newStatus of
        Draft ->
            StatusChangeEvent.view dtContext Icon.writingPad "Created Draft" details

        InReview ->
            let
                title =
                    case oldStatus of
                        Just Archived ->
                            "Re-opened"

                        _ ->
                            "Submitted for review"
            in
            StatusChangeEvent.view dtContext Icon.conversation title details

        Merged ->
            StatusChangeEvent.view dtContext Icon.merge "Merged" details

        Archived ->
            StatusChangeEvent.view dtContext Icon.archive "Archived" details


viewContributionEvent : AppContext -> ProjectRef -> ModifyCommentRequests -> ContributionEvent -> Html Msg
viewContributionEvent appContext projectRef modifyCommentRequests event =
    let
        actions commenter =
            CommentEvent.commentEventActions appContext
                { editMsg = EditComment
                , updateEditingMsg = UpdateEditingComment
                , saveEditMsg = SaveEditComment
                , cancelEditMsg = CancelEditComment
                , deleteMsg = ShowDeleteCommentConfirmation
                , confirmDeleteMsg = DeleteComment
                , cancelDeleteMsg = CancelDeleteComment
                }
                projectRef
                commenter
    in
    case event of
        ContributionEvent.StatusChange details ->
            div [ class "timeline-event" ]
                [ viewStatusChangeEvent appContext details ]

        ContributionEvent.Comment details ->
            let
                request =
                    modifyCommentRequests
                        |> Dict.get (CommentId.toString details.id)
                        |> Maybe.map .request
                        |> Maybe.withDefault CommentEvent.Idle
            in
            div [ class "timeline-event" ]
                [ CommentEvent.viewCommentEvent
                    appContext
                    (actions details.actor.handle)
                    { details = details, request = request }
                ]

        ContributionEvent.CommentRemoved details ->
            div [ class "timeline-event" ]
                [ CommentEvent.viewRemovedCommentEvent
                    appContext
                    details
                ]


view : AppContext -> ProjectRef -> Model -> Html Msg
view appContext projectRef model =
    case model.timeline of
        Success timeline ->
            div []
                [ div [ class "timeline" ] (List.map (viewContributionEvent appContext projectRef model.modifyCommentRequests) timeline)
                , CommentEvent.viewNewComment
                    appContext
                    { comment = model.newComment
                    , updateMsg = UpdateNewComment
                    , postMsg = PostNewComment
                    }
                ]

        _ ->
            div []
                [ Placeholder.text
                    |> Placeholder.view
                ]
