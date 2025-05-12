module UnisonShare.Notification exposing (..)

import Json.Decode as Decode exposing (field, string)
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (whenFieldIs)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.User as User exposing (UserSummary)


type NotificationSubject
    = TicketSubject TicketRef
    | ContributionSubject ContributionRef


type alias Notification =
    { id : String
    , projectRef : ProjectRef
    , title : String
    , subject : NotificationSubject
    , occurredAt : DateTime
    , participants : List UserSummary
    , isUnread : Bool
    }



-- DECODE


decode : Decode.Decoder Notification
decode =
    let
        decodeTicketSubject =
            Decode.map TicketSubject (field "ticketRef" TicketRef.decode)

        decodeContributionSubject =
            Decode.map ContributionSubject (field "contributionRef" ContributionRef.decode)

        decodeSubject =
            Decode.oneOf
                [ whenFieldIs "tag" "ticket" decodeTicketSubject
                , whenFieldIs "tag" "contribution" decodeContributionSubject
                ]
    in
    Decode.succeed Notification
        |> required "id" string
        |> required "projectRef" ProjectRef.decode
        |> required "title" string
        |> required "subject" decodeSubject
        |> required "occurredAt" DateTime.decode
        |> required "participants" (Decode.list User.decodeSummary)
        |> required "isUnread" Decode.bool
