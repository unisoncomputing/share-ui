module UnisonShare.ProjectAccess exposing (..)

import Json.Decode as Decode exposing (bool)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type ProjectAccess
    = Admin
    | Maintainer
    | Viewer


toString : ProjectAccess -> String
toString access =
    case access of
        Admin ->
            "Admin"

        Maintainer ->
            "Maintainer"

        Viewer ->
            "Viewer"


encodeNoAccess : Encode.Value
encodeNoAccess =
    Encode.object
        [ ( "canView", Encode.bool False )
        , ( "canMaintain", Encode.bool False )
        , ( "canAdmin", Encode.bool False )
        ]


encode : ProjectAccess -> Encode.Value
encode access =
    case access of
        Viewer ->
            Encode.object
                [ ( "canView", Encode.bool True )
                , ( "canMaintain", Encode.bool False )
                , ( "canAdmin", Encode.bool False )
                ]

        Maintainer ->
            Encode.object
                [ ( "canView", Encode.bool True )
                , ( "canMaintain", Encode.bool True )
                , ( "canAdmin", Encode.bool False )
                ]

        Admin ->
            Encode.object
                [ ( "canView", Encode.bool True )
                , ( "canMaintain", Encode.bool True )
                , ( "canAdmin", Encode.bool True )
                ]


decode : Decode.Decoder ProjectAccess
decode =
    let
        makeAccess canAdmin canMaintain _ =
            if canAdmin then
                Admin

            else if canMaintain then
                Maintainer

            else
                Viewer
    in
    Decode.succeed makeAccess
        |> required "canAdmin" bool
        |> required "canMaintain" bool
        |> required "canView" bool
