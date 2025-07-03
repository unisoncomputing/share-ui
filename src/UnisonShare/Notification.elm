module UnisonShare.Notification exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Version as Version exposing (Version)
import Json.Decode as Decode exposing (field, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Json.Encode as Encode
import Lib.Decode.Helpers exposing (maybeAt, whenFieldIs)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)
import UnisonShare.User as User exposing (UserSummaryWithId)


type NotificationStatus
    = Read
    | Unread
    | Archived


type NotificationEventData
    = ProjectContributionCreated
        { projectRef : ProjectRef
        , contributionAuthor : UserSummaryWithId
        , contributionRef : ContributionRef
        , description : Maybe String
        , title : String
        , status : ContributionStatus
        }
    | ProjectContributionUpdated
        { projectRef : ProjectRef
        , contributionAuthor : UserSummaryWithId
        , contributionRef : ContributionRef
        , description : Maybe String
        , title : String
        , status : ContributionStatus
        }
    | ProjectContributionComment
        { projectRef : ProjectRef
        , contributionAuthor : UserSummaryWithId
        , contributionRef : ContributionRef
        , description : Maybe String
        , title : String
        , status : ContributionStatus
        , comment : String
        , commentAuthor : UserSummaryWithId
        }
    | ProjectTicketCreated
        { projectRef : ProjectRef
        , ticketAuthor : UserSummaryWithId
        , ticketRef : TicketRef
        , description : Maybe String
        , title : String
        , status : TicketStatus
        }
    | ProjectTicketUpdated
        { projectRef : ProjectRef
        , ticketAuthor : UserSummaryWithId
        , ticketRef : TicketRef
        , description : Maybe String
        , title : String
        , status : TicketStatus
        }
    | ProjectTicketComment
        { projectRef : ProjectRef
        , ticketAuthor : UserSummaryWithId
        , ticketRef : TicketRef
        , description : Maybe String
        , title : String
        , status : TicketStatus
        , comment : String
        , commentAuthor : UserSummaryWithId
        }
    | ProjectBranchUpdated
        { projectRef : ProjectRef
        , branchRef : BranchRef
        }
    | ProjectReleaseCreated
        { projectRef : ProjectRef
        , version : Version
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



-- ENCODE


statusToString : NotificationStatus -> String
statusToString status =
    case status of
        Read ->
            "read"

        Unread ->
            "unread"

        Archived ->
            "archived"


encodeStatus : NotificationStatus -> Encode.Value
encodeStatus status =
    status
        |> statusToString
        |> Encode.string



-- DECODE


decodeStatus : Decode.Decoder NotificationStatus
decodeStatus =
    Decode.oneOf
        [ when string ((==) "unread") (Decode.succeed Unread)
        , when string ((==) "read") (Decode.succeed Read)
        , when string ((==) "archived") (Decode.succeed Archived)
        ]


decodeEventData : Decode.Decoder NotificationEventData
decodeEventData =
    let
        mkProjectContributionEvent ctor projectRef author contribRef description title status =
            ctor
                { projectRef = projectRef
                , contributionAuthor = author
                , contributionRef = contribRef
                , description = description
                , title = title
                , status = status
                }

        decodeContributionEvent ctor =
            Decode.succeed ctor
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "contribution", "author" ] User.decodeSummaryWithId
                |> requiredAt [ "contribution", "number" ] ContributionRef.decode
                |> maybeAt [ "contribution", "description" ] string
                |> requiredAt [ "contribution", "title" ] string
                |> requiredAt [ "contribution", "status" ] ContributionStatus.decode

        decodeContributionCreated =
            decodeContributionEvent (mkProjectContributionEvent ProjectContributionCreated)

        decodeContributionUpdated =
            decodeContributionEvent (mkProjectContributionEvent ProjectContributionUpdated)

        decodeContributionComment =
            let
                mkContributionComment projectRef author contribRef description title status comment commentAuthor =
                    ProjectContributionComment
                        { projectRef = projectRef
                        , contributionAuthor = author
                        , contributionRef = contribRef
                        , description = description
                        , title = title
                        , status = status
                        , comment = comment
                        , commentAuthor = commentAuthor
                        }
            in
            Decode.succeed mkContributionComment
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "contribution", "author" ] User.decodeSummaryWithId
                |> requiredAt [ "contribution", "number" ] ContributionRef.decode
                |> maybeAt [ "contribution", "description" ] string
                |> requiredAt [ "contribution", "title" ] string
                |> requiredAt [ "contribution", "status" ] ContributionStatus.decode
                |> requiredAt [ "comment", "content" ] string
                |> requiredAt [ "comment", "author" ] User.decodeSummaryWithId

        mkProjectTicketEvent ctor projectRef author contribRef description title status =
            ctor
                { projectRef = projectRef
                , ticketAuthor = author
                , ticketRef = contribRef
                , description = description
                , title = title
                , status = status
                }

        decodeTicketEvent ctor =
            Decode.succeed ctor
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "ticket", "author" ] User.decodeSummaryWithId
                |> requiredAt [ "ticket", "number" ] TicketRef.decode
                |> maybeAt [ "ticket", "description" ] string
                |> requiredAt [ "ticket", "title" ] string
                |> requiredAt [ "ticket", "status" ] TicketStatus.decode

        decodeTicketCreated =
            decodeTicketEvent (mkProjectTicketEvent ProjectTicketCreated)

        decodeTicketUpdated =
            decodeTicketEvent (mkProjectTicketEvent ProjectTicketUpdated)

        decodeTicketComment =
            let
                mkTicketComment projectRef author contribRef description title status comment commentAuthor =
                    ProjectTicketComment
                        { projectRef = projectRef
                        , ticketAuthor = author
                        , ticketRef = contribRef
                        , description = description
                        , title = title
                        , status = status
                        , comment = comment
                        , commentAuthor = commentAuthor
                        }
            in
            Decode.succeed mkTicketComment
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "ticket", "author" ] User.decodeSummaryWithId
                |> requiredAt [ "ticket", "number" ] TicketRef.decode
                |> maybeAt [ "ticket", "description" ] string
                |> requiredAt [ "ticket", "title" ] string
                |> requiredAt [ "ticket", "status" ] TicketStatus.decode
                |> requiredAt [ "comment", "content" ] string
                |> requiredAt [ "comment", "author" ] User.decodeSummaryWithId

        decodeBranchUpdated =
            Decode.succeed (\projectRef branchRef -> ProjectBranchUpdated { projectRef = projectRef, branchRef = branchRef })
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "branch", "branchShortHand" ] BranchRef.decode

        decodeReleaseCreated =
            Decode.succeed (\projectRef version -> ProjectReleaseCreated { projectRef = projectRef, version = version })
                |> requiredAt [ "project", "projectShortHand" ] ProjectRef.decode
                |> requiredAt [ "release", "version" ] Version.decode
    in
    Decode.oneOf
        [ whenFieldIs "kind" "project:contribution:created" (field "payload" decodeContributionCreated)
        , whenFieldIs "kind" "project:contribution:updated" (field "payload" decodeContributionUpdated)
        , whenFieldIs "kind" "project:contribution:comment" (field "payload" decodeContributionComment)
        , whenFieldIs "kind" "project:ticket:created" (field "payload" decodeTicketCreated)
        , whenFieldIs "kind" "project:ticket:updated" (field "payload" decodeTicketUpdated)
        , whenFieldIs "kind" "project:ticket:comment" (field "payload" decodeTicketComment)
        , whenFieldIs "kind" "project:branch:updated" (field "payload" decodeBranchUpdated)
        , whenFieldIs "kind" "project:release:created" (field "payload" decodeReleaseCreated)
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


decodeMaybe : Decode.Decoder (Maybe Notification)
decodeMaybe =
    Decode.oneOf
        [ Decode.map Just decode
        , Decode.succeed Nothing
        ]


decodeList : Decode.Decoder (List Notification)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
