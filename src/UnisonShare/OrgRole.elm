module UnisonShare.OrgRole exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Extra exposing (when)
import Json.Encode as Encode


type OrgRole
    = Owner
    | Admin
    | Maintainer
    | Contributor
    | Default
    | Viewer


toString : OrgRole -> String
toString role =
    case role of
        Owner ->
            "Owner"

        Admin ->
            "Admin"

        Maintainer ->
            "Maintainer"

        Contributor ->
            "Contributor"

        Default ->
            "Default"

        Viewer ->
            "Viewer"


encode : OrgRole -> Encode.Value
encode role =
    case role of
        Viewer ->
            Encode.string "org_viewer"

        Default ->
            Encode.string "org_default"

        Contributor ->
            Encode.string "org_contributor"

        Maintainer ->
            Encode.string "org_maintainer"

        Admin ->
            Encode.string "org_admin"

        Owner ->
            Encode.string "org_owner"


decode : Decode.Decoder OrgRole
decode =
    Decode.oneOf
        [ when string ((==) "org_owner") (Decode.succeed Owner)
        , when string ((==) "org_admin") (Decode.succeed Admin)
        , when string ((==) "org_maintainer") (Decode.succeed Maintainer)
        , when string ((==) "org_contributor") (Decode.succeed Contributor)
        , when string ((==) "org_default") (Decode.succeed Default)
        , when string ((==) "org_viewer") (Decode.succeed Viewer)
        ]
