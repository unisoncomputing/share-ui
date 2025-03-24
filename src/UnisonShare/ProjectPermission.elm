module UnisonShare.ProjectPermission exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Extra exposing (when)


type ProjectPermission
    = ProjectView
    | ProjectContribute
    | ProjectManage
    | ProjectDelete


decode : Decode.Decoder ProjectPermission
decode =
    Decode.oneOf
        [ when string ((==) "project:view") (Decode.succeed ProjectView)
        , when string ((==) "project:contribute") (Decode.succeed ProjectContribute)
        , when string ((==) "project:manage") (Decode.succeed ProjectManage)
        , when string ((==) "project:delete") (Decode.succeed ProjectDelete)
        ]
