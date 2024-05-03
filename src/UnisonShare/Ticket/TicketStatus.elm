module UnisonShare.Ticket.TicketStatus exposing
    ( TicketStatus(..)
    , decode
    , toApiString
    )

import Json.Decode as Decode
import Json.Decode.Extra exposing (when)


type TicketStatus
    = Open
    | Closed


toApiString : TicketStatus -> String
toApiString status =
    case status of
        Open ->
            "open"

        Closed ->
            "closed"



-- DECODE


decode : Decode.Decoder TicketStatus
decode =
    Decode.oneOf
        [ when Decode.string ((==) "open") (Decode.succeed Open)
        , when Decode.string ((==) "closed") (Decode.succeed Closed)
        ]
