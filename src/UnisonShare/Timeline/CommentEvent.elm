module UnisonShare.Timeline.CommentEvent exposing (..)

import Html exposing (Html, div, footer, h2, header, span, text)
import Html.Attributes exposing (class, classList)
import Http
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Lib.UserHandle exposing (UserHandle)
import UI
import UI.Button as Button
import UI.ByAt as ByAt
import UI.Card as Card
import UI.DateTime as DateTime exposing (DateTime)
import UI.Divider as Divider
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.StatusBanner as StatusBanner
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Markdown as Markdown
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Session as Session
import UnisonShare.Timeline.CommentId as CommentId exposing (CommentId)
import UnisonShare.Timeline.TimelineEvent as TimelineEvent exposing (TimelineEventDetails)
import UnisonShare.User as User exposing (UserSummary)



-- MODEL


type CommentRevision
    = Initial
    | Edited
        { revisionNumber : Int
        , editedAt : DateTime
        , editedBy : UserSummary
        }


type alias CommentDetails =
    TimelineEventDetails
        { id : CommentId
        , content : String
        , revision : Int
        }


type alias RemovedCommentDetails =
    { id : CommentId
    , timestamp : DateTime
    , deletedAt : DateTime
    }


type ModifyCommentRequest
    = Idle
    | ConfirmDelete
    | Deleting
    | DeleteFailure Http.Error
    | Deleted
    | Editing String
    | SavingEdit String
    | EditFailure Http.Error
    | EditSaved


type alias ModifiableComment =
    { details : CommentDetails
    , request : ModifyCommentRequest
    }


type CommentEvent
    = CommentEvent ModifiableComment
    | RemovedCommentEvent RemovedCommentDetails



-- VIEW


type alias ActionMsgs msg =
    { editMsg : CommentId -> msg
    , updateEditingMsg : CommentId -> String -> msg
    , saveEditMsg : CommentId -> String -> msg
    , cancelEditMsg : CommentId -> msg
    , deleteMsg : CommentId -> msg
    , confirmDeleteMsg : CommentId -> msg
    , cancelDeleteMsg : CommentId -> msg
    }


type CommentEventActions msg
    = NoModifyAccess
    | CanModify (ActionMsgs msg)


commentEventActions : AppContext -> ActionMsgs msg -> ProjectRef -> UserHandle -> CommentEventActions msg
commentEventActions appContext msgs projectRef commenterHandle =
    let
        isCommentOwner =
            Session.isHandle commenterHandle appContext.session

        canEditAndDelete =
            Session.hasProjectAccess projectRef appContext.session || isCommentOwner
    in
    if canEditAndDelete then
        CanModify msgs

    else
        NoModifyAccess


viewCommentEvent : AppContext -> CommentEventActions msg -> ModifiableComment -> Html msg
viewCommentEvent appContext actions { details, request } =
    let
        byAt =
            ByAt.byAt details.actor details.timestamp
                |> ByAt.withToClick Link.userProfile
                |> ByAt.view appContext.timeZone appContext.now

        md comment =
            Card.card
                [ Markdown.view comment ]
                |> Card.asContained
                |> Card.withTightPadding
                |> Card.withClassName "comment-event_content"
                |> Card.view

        currentComment =
            md details.content

        editAndDeleteActions editMsg deleteMsg =
            div [ class "event-actions" ]
                [ Button.icon (editMsg details.id) Icon.writingPad
                    |> Button.small
                    |> Button.subdued
                    |> Button.view
                , Button.icon (deleteMsg details.id) Icon.trash
                    |> Button.small
                    |> Button.subdued
                    |> Button.view
                ]

        ( content, status, actions_ ) =
            case ( actions, request ) of
                ( CanModify { confirmDeleteMsg, cancelDeleteMsg }, ConfirmDelete ) ->
                    ( currentComment
                    , UI.nothing
                    , div [ class "event-actions" ]
                        [ Button.icon (cancelDeleteMsg details.id) Icon.x
                            |> Button.small
                            |> Button.subdued
                            |> Button.view
                        , Button.button (confirmDeleteMsg details.id) "Delete"
                            |> Button.small
                            |> Button.emphasized
                            |> Button.view
                        ]
                    )

                ( _, Deleting ) ->
                    ( currentComment, StatusBanner.working "Deleting...", UI.nothing )

                ( CanModify { editMsg, deleteMsg }, DeleteFailure _ ) ->
                    ( currentComment
                    , StatusBanner.bad "Something happened on our end and the comment wasn't deleted."
                    , editAndDeleteActions editMsg deleteMsg
                    )

                ( _, Deleted ) ->
                    ( UI.nothing, UI.nothing, UI.nothing )

                ( CanModify { updateEditingMsg, cancelEditMsg, saveEditMsg }, Editing edit ) ->
                    ( div [ class "comment-event_edit" ]
                        [ TextField.fieldWithoutLabel
                            (updateEditingMsg details.id)
                            "Be kind and respectful."
                            edit
                            |> TextField.withRows 4
                            |> TextField.withId ("edit_" ++ CommentId.toString details.id)
                            |> TextField.view
                        ]
                    , UI.nothing
                    , div [ class "event-actions" ]
                        [ Button.icon (cancelEditMsg details.id) Icon.x
                            |> Button.small
                            |> Button.subdued
                            |> Button.view
                        , Button.button (saveEditMsg details.id edit) "Save"
                            |> Button.small
                            |> Button.emphasized
                            |> Button.view
                        ]
                    )

                ( _, SavingEdit edit ) ->
                    ( md edit, StatusBanner.working "Saving...", UI.nothing )

                ( CanModify { editMsg, deleteMsg }, EditFailure _ ) ->
                    ( currentComment
                    , StatusBanner.bad "Something happened on our end and the comment wasn't saved."
                    , editAndDeleteActions editMsg deleteMsg
                    )

                ( CanModify { editMsg, deleteMsg }, _ ) ->
                    ( currentComment, UI.nothing, editAndDeleteActions editMsg deleteMsg )

                _ ->
                    ( currentComment, UI.nothing, UI.nothing )
    in
    div [ class "comment-event" ]
        [ header [ class "timeline-event_header" ]
            [ div [ class "timeline-event_header_description" ]
                [ TimelineEvent.viewIcon Icon.speechBubbleFromRight
                , byAt
                , status
                ]
            , actions_
            ]
        , content
        ]


viewRemovedCommentEvent : AppContext -> RemovedCommentDetails -> Html msg
viewRemovedCommentEvent appContext { timestamp, deletedAt } =
    div [ class "removed-comment-event" ]
        [ header [ class "timeline-event_header" ]
            [ div [ class "timeline-event_header_description" ]
                [ TimelineEvent.viewIcon Icon.speechBubbleFromRightOutlined
                , TimelineEvent.viewDescription [ TimelineEvent.viewTitle "Comment removed" ]
                , span [ class "by-at" ]
                    [ text "Posted "
                    , DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone timestamp
                    , span []
                        [ text "and deleted "
                        , DateTime.view (DateTime.DistanceFrom appContext.now) appContext.timeZone deletedAt
                        ]
                    ]
                ]
            ]
        ]


type NewComment
    = WritingComment String
    | PostingComment String
    | CommentPostingFailure String Http.Error
    | CommentPostingSuccess CommentDetails


type alias NewCommentConfig msg =
    { updateMsg : String -> msg
    , postMsg : msg
    , comment : NewComment
    }


viewNewComment : AppContext -> NewCommentConfig msg -> Html msg
viewNewComment appContext { updateMsg, postMsg, comment } =
    let
        viewField txt =
            TextField.fieldWithoutLabel updateMsg
                "Be kind and respectful."
                txt
                |> TextField.withRows 4
                |> TextField.view

        postButton =
            Button.button postMsg "Post comment"
                |> Button.emphasized
                |> Button.view
    in
    case appContext.session of
        Session.SignedIn _ ->
            let
                ( commentField, actions, working ) =
                    case comment of
                        WritingComment txt ->
                            ( viewField txt
                            , footer [ class "comment-actions" ] [ postButton ]
                            , False
                            )

                        PostingComment text ->
                            ( viewField text
                            , footer [ class "comment-actions" ]
                                [ StatusBanner.working "Posting..."
                                , postButton
                                ]
                            , True
                            )

                        CommentPostingSuccess _ ->
                            ( viewField ""
                            , footer [ class "comment-actions" ]
                                [ StatusBanner.good "New comment posted"
                                , postButton
                                ]
                            , False
                            )

                        CommentPostingFailure txt _ ->
                            ( viewField txt
                            , footer [ class "comment-actions" ]
                                [ StatusBanner.bad "Couldn't post comment, please try agan"
                                , postButton
                                ]
                            , False
                            )
            in
            div [ class "new-comment_form", classList [ ( "new-comment_form_working", working ) ] ]
                [ Divider.divider |> Divider.small |> Divider.withoutMargin |> Divider.view
                , h2 [] [ text "Post a comment" ]
                , commentField
                , actions
                ]

        _ ->
            UI.nothing



-- DECODE


{-| TODO
-}
decodeRevision : Decode.Decoder CommentRevision
decodeRevision =
    let
        makeRevisionDetails revNumber editedAt editedBy =
            { revisionNumber = revNumber, editedAt = editedAt, editedBy = editedBy }
    in
    Decode.oneOf
        [ when (Decode.field "revision" Decode.int) ((==) 0) (Decode.succeed Initial)
        , Decode.map Edited
            (Decode.succeed makeRevisionDetails
                |> required "revision" Decode.int
                |> required "editedAt" DateTime.decode
                |> required "editedBy" User.decodeSummary
            )
        ]


decodeCommentDetails : Decode.Decoder CommentDetails
decodeCommentDetails =
    let
        makeCommentDetails id content timestamp actor revision =
            { id = id
            , content = content
            , timestamp = timestamp
            , actor = actor
            , revision = revision
            }
    in
    Decode.succeed makeCommentDetails
        |> required "id" CommentId.decode
        |> required "content" Decode.string
        |> required "timestamp" DateTime.decode
        |> required "actor" User.decodeSummary
        |> required "revision" Decode.int


decodeRemovedCommentDetails : Decode.Decoder RemovedCommentDetails
decodeRemovedCommentDetails =
    let
        makeCommentRemovedDetails id deletedAt timestamp =
            { id = id
            , deletedAt = deletedAt
            , timestamp = timestamp
            }
    in
    Decode.succeed makeCommentRemovedDetails
        |> required "id" CommentId.decode
        |> required "deletedAt" DateTime.decode
        |> required "timestamp" DateTime.decode
