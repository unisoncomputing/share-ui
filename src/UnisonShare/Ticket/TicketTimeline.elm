module UnisonShare.Ticket.TicketTimeline exposing (..)

import Browser.Dom as Dom
import Dict exposing (Dict)
import Html exposing (Html, div, text)
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
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketEvent as TicketEvent exposing (TicketEvent(..))
import UnisonShare.Ticket.TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus exposing (TicketStatus(..))
import UnisonShare.Timeline.CommentEvent as CommentEvent exposing (CommentDetails)
import UnisonShare.Timeline.CommentId as CommentId exposing (CommentId)
import UnisonShare.Timeline.StatusChangeEvent as StatusChangeEvent
import UnisonShare.Timeline.TimelineEvent as TimelineEvent


type alias TicketTimeline =
    List TicketEvent


type alias ModifyCommentRequests =
    Dict
        -- This is a CommentId
        String
        { original : CommentDetails
        , request : CommentEvent.ModifyCommentRequest
        }


type alias Model =
    { timeline : WebData TicketTimeline
    , modifyCommentRequests : ModifyCommentRequests
    , newComment : CommentEvent.NewComment
    }


init : AppContext -> ProjectRef -> TicketRef -> ( Model, Cmd Msg )
init appContext projectRef contribRef =
    ( { timeline = Loading
      , modifyCommentRequests = Dict.empty
      , newComment = CommentEvent.WritingComment ""
      }
    , fetchTicketTimeline appContext projectRef contribRef
    )



-- UPDATE


type Msg
    = NoOp
    | FetchTicketTimelineFinished (WebData TicketTimeline)
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


update : AppContext -> ProjectRef -> TicketRef -> Msg -> Model -> ( Model, Cmd Msg )
update appContext projectRef contribRef msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchTicketTimelineFinished timeline ->
            ( { model | timeline = timeline }, Cmd.none )

        UpdateNewComment text ->
            ( { model | newComment = CommentEvent.WritingComment text }, Cmd.none )

        PostNewComment ->
            case model.newComment of
                CommentEvent.WritingComment text ->
                    if not (String.isEmpty text) then
                        ( { model | newComment = CommentEvent.PostingComment text }
                        , postTicketComment appContext projectRef contribRef text
                        )

                    else
                        ( model, Cmd.none )

                CommentEvent.CommentPostingFailure text _ ->
                    if not (String.isEmpty text) then
                        ( { model | newComment = CommentEvent.PostingComment text }
                        , postTicketComment appContext projectRef contribRef text
                        )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        PostNewCommentFinished _ (Ok comment) ->
            let
                commentEvent =
                    TicketEvent.Comment comment

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
                    , updateTicketComment appContext projectRef contribRef id original.revision edit
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveEditCommentFinished id _ (Ok comment) ->
            let
                replaceComment evt =
                    case evt of
                        TicketEvent.Comment c ->
                            if CommentId.equals c.id id then
                                TicketEvent.Comment comment

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
                    , deleteTicketComment
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


addEvent : Model -> TicketEvent -> Model
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


fetchTicketTimeline : AppContext -> ProjectRef -> TicketRef -> Cmd Msg
fetchTicketTimeline appContext projectRef contributionRef =
    ShareApi.projectTicketTimeline projectRef contributionRef
        |> HttpApi.toRequest
            (Decode.field "items" (Decode.list TicketEvent.decode))
            (RemoteData.fromResult >> FetchTicketTimelineFinished)
        |> HttpApi.perform appContext.api


postTicketComment : AppContext -> ProjectRef -> TicketRef -> String -> Cmd Msg
postTicketComment appContext projectRef contributionRef text =
    ShareApi.createProjectTicketComment projectRef contributionRef text
        |> HttpApi.toRequest CommentEvent.decodeCommentDetails (PostNewCommentFinished text)
        |> HttpApi.perform appContext.api


updateTicketComment : AppContext -> ProjectRef -> TicketRef -> CommentId -> Int -> String -> Cmd Msg
updateTicketComment appContext projectRef contributionRef commentId originalRevision text =
    ShareApi.updateProjectTicketComment projectRef contributionRef commentId originalRevision text
        |> HttpApi.toRequest (Decode.field "comment" CommentEvent.decodeCommentDetails) (SaveEditCommentFinished commentId text)
        |> HttpApi.perform appContext.api


deleteTicketComment : AppContext -> ProjectRef -> TicketRef -> CommentId -> Cmd Msg
deleteTicketComment appContext projectRef contributionRef commentId =
    ShareApi.deleteProjectTicketComment projectRef contributionRef commentId
        |> HttpApi.toRequestWithEmptyResponse
            (RemoteData.fromResult >> DeleteCommentFinished commentId)
        |> HttpApi.perform appContext.api



-- VIEW


viewStatusChangeEvent : DateTimeContext a -> TicketEvent.StatusChangeDetails -> Html Msg
viewStatusChangeEvent dtContext ({ newStatus, oldStatus } as details) =
    case newStatus of
        Open ->
            let
                title =
                    case oldStatus of
                        Just Closed ->
                            "Re-opened"

                        _ ->
                            "Opened"
            in
            StatusChangeEvent.view dtContext Icon.conversation title details

        Closed ->
            StatusChangeEvent.view dtContext Icon.archive "Closed" details


viewTicketEvent : AppContext -> ProjectRef -> ModifyCommentRequests -> TicketEvent -> Html Msg
viewTicketEvent appContext projectRef modifyCommentRequests event =
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
        TicketEvent.StatusChange details ->
            TimelineEvent.view
                [ viewStatusChangeEvent appContext details ]

        TicketEvent.Comment details ->
            let
                request =
                    modifyCommentRequests
                        |> Dict.get (CommentId.toString details.id)
                        |> Maybe.map .request
                        |> Maybe.withDefault CommentEvent.Idle
            in
            TimelineEvent.view
                [ CommentEvent.viewCommentEvent
                    appContext
                    (actions details.actor.handle)
                    { details = details, request = request }
                ]

        TicketEvent.CommentRemoved details ->
            TimelineEvent.view
                [ CommentEvent.viewRemovedCommentEvent
                    appContext
                    details
                ]


view : AppContext -> ProjectRef -> Model -> Html Msg
view appContext projectRef model =
    let
        shape length =
            Placeholder.text
                |> Placeholder.withLength length
                |> Placeholder.subdued
                |> Placeholder.tiny
                |> Placeholder.view

        viewLoading =
            div [ class "loading-timeline" ]
                [ shape Placeholder.Large
                , shape Placeholder.Small
                , shape Placeholder.Medium
                ]
    in
    case model.timeline of
        NotAsked ->
            viewLoading

        Loading ->
            viewLoading

        Success timeline ->
            div []
                [ div [ class "timeline" ] (List.map (viewTicketEvent appContext projectRef model.modifyCommentRequests) timeline)
                , CommentEvent.viewNewComment
                    appContext
                    { comment = model.newComment
                    , updateMsg = UpdateNewComment
                    , postMsg = PostNewComment
                    }
                ]

        Failure _ ->
            div [ class "error" ] [ text "Error loading timeline." ]
