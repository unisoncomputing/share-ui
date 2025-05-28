module UnisonShare.BranchDiff.LibDep exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (failInvalid, whenTagIs)


type alias LibDepInfo =
    { name : String }


type LibDep
    = Added LibDepInfo
    | Removed LibDepInfo


decodeMaybe : Decode.Decoder (Maybe LibDep)
decodeMaybe =
    let
        makeAdded name =
            Added { name = name }

        makeRemoved name =
            Removed { name = name }
    in
    Decode.oneOf
        [ whenTagIs "Added"
            (Decode.map Just
                (Decode.succeed makeAdded
                    |> required "name" Decode.string
                )
            )
        , whenTagIs "Removed"
            (Decode.map Just
                (Decode.succeed makeRemoved
                    |> required "name" Decode.string
                )
            )
        , Decode.succeed Nothing
        ]


decode : Decode.Decoder LibDep
decode =
    decodeMaybe
        |> Decode.andThen (failInvalid "Invalid Lib dependency")


decodeList : Decode.Decoder (List LibDep)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
