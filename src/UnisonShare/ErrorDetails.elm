module UnisonShare.ErrorDetails exposing (..)

import Html exposing (Html, details, pre, summary, text)
import Http
import Lib.Util as Util
import UI
import UnisonShare.Session as Session exposing (Session)


view : Session -> Http.Error -> Html msg
view session error =
    if Session.isSuperAdmin session then
        details []
            [ summary [] [ text "Error Details" ]
            , pre [] [ text (Util.httpErrorToString error) ]
            ]

    else
        UI.nothing
