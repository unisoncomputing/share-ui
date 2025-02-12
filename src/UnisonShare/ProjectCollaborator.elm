module UnisonShare.ProjectCollaborator exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import UnisonShare.ProjectAccess as ProjectAccess exposing (ProjectAccess)
import UnisonShare.User as User exposing (UserSummaryWithId)


type alias ProjectCollaborator =
    { access : ProjectAccess
    , user : UserSummaryWithId
    }


decode : Decode.Decoder ProjectCollaborator
decode =
    Decode.succeed ProjectCollaborator
        |> required "permissions" ProjectAccess.decode
        |> required "user" User.decodeSummaryWithId
