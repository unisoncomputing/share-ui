module UnisonShare.Org exposing
    ( Org
    , OrgSummary
    , OrgWithPermissions
    , can
    , canAdmin
    , canChangeOwner
    , canCreateProject
    , canDelete
    , canManage
    , canView
    , decodeSummary
    , name
    , toAvatar
    )

import Json.Decode as Decode exposing (field, maybe, string)
import Lib.Decode.Helpers exposing (url)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import UnisonShare.OrgPermission as OrgPermission exposing (OrgPermission)
import Url exposing (Url)


type alias Org u =
    { u
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
    }


type alias OrgSummary =
    Org { permissions : List OrgPermission }



-- HELPERS


name : Org u -> String
name user =
    Maybe.withDefault (UserHandle.toString user.handle) user.name


toAvatar : Org u -> Avatar msg
toAvatar user =
    Avatar.avatar user.avatarUrl (Just (name user))
        |> Avatar.withIcon Icon.user



-- PERMISSIONS


type alias OrgWithPermissions o =
    Org { o | permissions : List OrgPermission }


can : OrgPermission -> OrgWithPermissions a -> Bool
can permission org =
    List.member permission org.permissions


canView : OrgWithPermissions a -> Bool
canView org =
    can OrgPermission.OrgView org


canManage : OrgWithPermissions a -> Bool
canManage org =
    can OrgPermission.OrgManage org


canAdmin : OrgWithPermissions a -> Bool
canAdmin org =
    can OrgPermission.OrgAdmin org


canCreateProject : OrgWithPermissions a -> Bool
canCreateProject org =
    can OrgPermission.OrgCreateProject org


canDelete : OrgWithPermissions a -> Bool
canDelete org =
    can OrgPermission.OrgDelete org


canChangeOwner : OrgWithPermissions a -> Bool
canChangeOwner org =
    can OrgPermission.OrgChangeOwner org



-- DECODE


decodeSummary : Decode.Decoder OrgSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl permissions =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , permissions = permissions
            }
    in
    Decode.map4 makeSummary
        (field "handle" UserHandle.decodeUnprefixed)
        (maybe (field "name" string))
        (maybe (field "avatarUrl" url))
        (field "permissions" OrgPermission.decodeList)
