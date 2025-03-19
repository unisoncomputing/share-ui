module UnisonShare.ProjectRole exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Extra exposing (when)
import Json.Encode as Encode


type ProjectRole
    = Owner
    | Admin
    | Maintainer
    | Viewer


toString : ProjectRole -> String
toString role =
    case role of
        Owner ->
            "Owner"

        Admin ->
            "Admin"

        Maintainer ->
            "Maintainer"

        Viewer ->
            "Viewer"


encode : ProjectRole -> Encode.Value
encode role =
    case role of
        Viewer ->
            Encode.string "project_viewer"

        Maintainer ->
            Encode.string "project_contributor"

        Admin ->
            Encode.string "project_admin"

        Owner ->
            Encode.string "project_owner"


decode : Decode.Decoder ProjectRole
decode =
    Decode.oneOf
        [ when string ((==) "project_owner") (Decode.succeed Owner)
        , when string ((==) "project_admin") (Decode.succeed Admin)
        , when string ((==) "project_contributor") (Decode.succeed Maintainer)
        , when string ((==) "project_viewer") (Decode.succeed Viewer)
        ]
