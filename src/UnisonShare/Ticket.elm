module UnisonShare.Ticket exposing (Ticket, decode)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)


type alias Ticket =
    { ref : TicketRef
    , authorHandle : Maybe UserHandle
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
    Decode.succeed Ticket
        |> required "number" TicketRef.decode
        |> optional "author" (Decode.map Just UserHandle.decodeUnprefixed) Nothing
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" TicketStatus.decode
        |> required "numComments" Decode.int
        |> required "title" Decode.string
        |> required "description" Decode.string
