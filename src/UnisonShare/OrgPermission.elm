module UnisonShare.OrgPermission exposing (..)

import Json.Decode as Decode
import Lib.Util as Util


type OrgPermission
    = OrgView
    | OrgManage
    | OrgAdmin
    | OrgCreateProject
    | OrgDelete
    | OrgChangeOwner


fromString : String -> Maybe OrgPermission
fromString raw =
    case raw of
        "org:view" ->
            Just OrgView

        "org:manage" ->
            Just OrgManage

        "org:admin" ->
            Just OrgAdmin

        "org:create_project" ->
            Just OrgCreateProject

        "org:delete" ->
            Just OrgDelete

        "org:change_owner" ->
            Just OrgChangeOwner

        _ ->
            Nothing


decode : Decode.Decoder OrgPermission
decode =
    decodeMaybe
        |> Decode.andThen (Util.decodeFailInvalid "Invalid OrgPermission")


decodeMaybe : Decode.Decoder (Maybe OrgPermission)
decodeMaybe =
    Decode.map fromString Decode.string


{-| soft fail when encountering unknown permissions
-}
decodeList : Decode.Decoder (List OrgPermission)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
