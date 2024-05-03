module UnisonShare.SupportChatWidget exposing (..)

import Html exposing (Html, node)
import Html.Attributes exposing (attribute)
import Lib.UserHandle as UserHandle
import Maybe.Extra as MaybeE
import UnisonShare.Account exposing (Account)
import Url


view : Account a -> Html msg
view account =
    node "support-chat-widget"
        (MaybeE.values
            [ Maybe.map (attribute "name") account.name
            , Just (attribute "handle" (UserHandle.toUnprefixedString account.handle))
            , Maybe.map (Url.toString >> attribute "avatar-url") account.avatarUrl
            ]
        )
        []
