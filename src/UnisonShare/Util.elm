module UnisonShare.Util exposing (..)

import Html exposing (Html)
import UI
import UnisonShare.Account exposing (Account)


privateBeta : Account a -> Html msg -> Html msg
privateBeta account content =
    if account.isSuperAdmin then
        content

    else
        UI.nothing
