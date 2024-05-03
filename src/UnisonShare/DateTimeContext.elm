module UnisonShare.DateTimeContext exposing (DateTimeContext)

import Time
import UI.DateTime exposing (DateTime)


type alias DateTimeContext a =
    { a
        | now : DateTime
        , timeZone : Time.Zone
    }
