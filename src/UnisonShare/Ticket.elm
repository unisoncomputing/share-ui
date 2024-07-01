module UnisonShare.Ticket exposing (Ticket, decode)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)
import UnisonShare.User as User exposing (UserSummary)


type alias Ticket =
    { ref : TicketRef
    , author : Maybe UserSummary
    , projectRef : ProjectRef
    , createdAt : DateTime
    , updatedAt : DateTime
    , status : TicketStatus
    , numComments : Int
    , title : String
    , description : String
    }



-- DECODE


decode : Decode.Decoder Ticket
decode =
    let
        emptyUser h =
            { handle = h
            , name = Nothing
            , avatarUrl = Nothing
            , pronouns = Nothing
            }

        decodeAuthor =
            Decode.oneOf
                [ Decode.map emptyUser UserHandle.decodeUnprefixed
                , User.decodeSummary
                ]
    in
    Decode.succeed Ticket
        |> required "number" TicketRef.decode
        |> optional "author" (Decode.map Just decodeAuthor) Nothing
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" TicketStatus.decode
        |> required "numComments" Decode.int
        |> required "title" Decode.string
        |> required "description" Decode.string
