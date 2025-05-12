module UnisonShare.ProjectPermission exposing (..)

import Json.Decode as Decode
import Lib.Decode.Helpers exposing (failInvalid)


type ProjectPermission
    = ProjectView
    | ProjectContribute
    | ProjectMaintain
    | ProjectManage
    | ProjectDelete


fromString : String -> Maybe ProjectPermission
fromString raw =
    case raw of
        "project:view" ->
            Just ProjectView

        "project:contribute" ->
            Just ProjectContribute

        "project:maintain" ->
            Just ProjectMaintain

        "project:manage" ->
            Just ProjectManage

        "project:delete" ->
            Just ProjectDelete

        _ ->
            Nothing


decode : Decode.Decoder ProjectPermission
decode =
    decodeMaybe
        |> Decode.andThen (failInvalid "Invalid ProjectPermission")


decodeMaybe : Decode.Decoder (Maybe ProjectPermission)
decodeMaybe =
    Decode.map fromString Decode.string


{-| soft fail when encountering unknown permissions
-}
decodeList : Decode.Decoder (List ProjectPermission)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
