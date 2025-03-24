module UnisonShare.Org exposing
    ( Org
    , OrgSummary
    , decodeSummary
    , name
    , toAvatar
    )

import Json.Decode as Decode exposing (field, maybe, string)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util exposing (decodeUrl)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import Url exposing (Url)


type alias Org u =
    { u
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


type alias OrgSummary =
    Org {}



-- HELPERS


name : Org u -> String
name user =
    Maybe.withDefault (UserHandle.toString user.handle) user.name


toAvatar : Org u -> Avatar msg
toAvatar user =
    Avatar.avatar user.avatarUrl (Just (name user))
        |> Avatar.withIcon Icon.user



-- DECODE


decodeSummary : Decode.Decoder OrgSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            }
    in
    Decode.map3 makeSummary
        (field "handle" UserHandle.decodeUnprefixed)
        (maybe (field "name" string))
        (maybe (field "avatarUrl" decodeUrl))
