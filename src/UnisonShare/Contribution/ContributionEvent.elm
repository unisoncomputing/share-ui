module UnisonShare.Contribution.ContributionEvent exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (optional, required)
import UI.DateTime as DateTime
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Timeline.CommentEvent as CommentEvent exposing (CommentDetails, RemovedCommentDetails)
import UnisonShare.Timeline.TimelineEvent exposing (TimelineEventDetails)
import UnisonShare.User as User


type ContributionEvent
    = StatusChange StatusChangeDetails
    | Comment CommentDetails
    | CommentRemoved RemovedCommentDetails


type alias StatusChangeDetails =
    TimelineEventDetails
        { newStatus : ContributionStatus

        -- TODO: Better support the initial change, which currently is implicitly
        -- implied by this Maybe
        , oldStatus : Maybe ContributionStatus
        }


decodeStatusChangeDetails : Decode.Decoder StatusChangeDetails
decodeStatusChangeDetails =
    let
        makeStatusChangeDetails newStatus oldStatus timestamp actor =
            { newStatus = newStatus
            , oldStatus = oldStatus
            , timestamp = timestamp
            , actor = actor
            }
    in
    Decode.succeed makeStatusChangeDetails
        |> required "newStatus" ContributionStatus.decode
        |> optional "oldStatus" (Decode.map Just ContributionStatus.decode) Nothing
        |> required "timestamp" DateTime.decode
        |> required "actor" User.decodeSummary


decode : Decode.Decoder ContributionEvent
decode =
    Decode.oneOf
        [ when (Decode.field "kind" Decode.string) ((==) "statusChange") (Decode.map StatusChange decodeStatusChangeDetails)
        , when (Decode.field "kind" Decode.string) ((==) "comment") (Decode.map Comment CommentEvent.decodeCommentDetails)
        , when (Decode.field "kind" Decode.string) ((==) "comment") (Decode.map CommentRemoved CommentEvent.decodeRemovedCommentDetails)
        ]
