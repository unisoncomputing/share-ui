module UnisonShare.Org exposing
    ( Org
    , OrgDetails
    , OrgSummary
    , OrgWithPermissions
    , can
    , canAdmin
    , canChangeOwner
    , canCreateProject
    , canDelete
    , canManage
    , canView
    , decodeDetails
    , decodeSummary
    , name
    , toAvatar
    )

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (maybeAt, url)
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
    Org {}


type alias OrgDetails =
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


decodeDetails : Decode.Decoder OrgDetails
decodeDetails =
    let
        makeDetails handle name_ avatarUrl permissions =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , permissions = permissions
            }
    in
    Decode.succeed makeDetails
        |> requiredAt [ "user", "handle" ] UserHandle.decodeUnprefixed
        |> maybeAt [ "user", "name" ] string
        |> maybeAt [ "user", "avatarUrl" ] url
        |> required "permissions" OrgPermission.decodeList


decodeSummary : Decode.Decoder OrgSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            }
    in
    Decode.succeed makeSummary
        |> requiredAt [ "user", "handle" ] UserHandle.decodeUnprefixed
        |> maybeAt [ "user", "name" ] string
        |> maybeAt [ "user", "avatarUrl" ] url
