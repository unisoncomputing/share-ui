module UnisonShare.Notification exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Json.Decode as Decode exposing (field, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (maybeAt, whenFieldIs)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.User as User exposing (UserSummaryWithId)


type NotificationStatus
    = Read
    | Unread
    | Archived


type NotificationEventData
    = ProjectContributionCreated
        { projectRef : ProjectRef
        , author : UserSummaryWithId
        , contributionRef : ContributionRef
        , description : String
        , title : String
        , status : ContributionStatus
        }
    | ProjectBranchUpdated
        { projectRef : ProjectRef
        , branchRef : BranchRef
        }


type alias NotificationEvent =
    { id : String
    , occurredAt : DateTime
    , data : NotificationEventData
    , actor : Maybe UserSummaryWithId
    }


type alias Notification =
    { id : String
    , status : NotificationStatus
    , event : NotificationEvent
    }



-- HELPERS


isUnread : Notification -> Bool
isUnread notification =
    notification.status == Unread



-- DECODE


decodeStatus : Decode.Decoder NotificationStatus
decodeStatus =
    Decode.oneOf
        [ when string ((==) "unread") (Decode.succeed Read)
        , when string ((==) "read") (Decode.succeed Unread)
        , when string ((==) "archived") (Decode.succeed Archived)
        ]


decodeEventData : Decode.Decoder NotificationEventData
decodeEventData =
    let
        mkProjectContributionCreated projectRef author contribRef description title status =
            ProjectContributionCreated
                { projectRef = projectRef
                , author = author
                , contributionRef = contribRef
                , description = description
                , title = title
                , status = status
                }

        decodeContributionCreated =
            Decode.succeed mkProjectContributionCreated
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> required "author" User.decodeSummaryWithId
                |> required "number" ContributionRef.decode
                |> required "description" string
                |> required "title" string
                |> required "status" ContributionStatus.decode

        decodeBranchUpdated =
            Decode.succeed (\projectRef branchRef -> ProjectBranchUpdated { projectRef = projectRef, branchRef = branchRef })
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "branch", "branchShortHand" ] BranchRef.decode
    in
    Decode.oneOf
        [ whenFieldIs "kind" "projectContributionCreated" (field "payload" decodeContributionCreated)
        , whenFieldIs "kind" "projectBranchUpdated" (field "payload" decodeBranchUpdated)
        ]


decodeEvent : Decode.Decoder NotificationEvent
decodeEvent =
    Decode.succeed NotificationEvent
        |> required "id" string
        |> required "occurredAt" DateTime.decode
        |> required "data" decodeEventData
        |> maybeAt [ "actor", "info" ] User.decodeSummaryWithId


decode : Decode.Decoder Notification
decode =
    Decode.succeed Notification
        |> required "id" string
        |> required "status" decodeStatus
        |> required "event" decodeEvent
