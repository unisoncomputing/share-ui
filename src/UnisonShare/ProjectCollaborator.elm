module UnisonShare.ProjectCollaborator exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, requiredAt)
import UnisonShare.ProjectRole as ProjectRole exposing (ProjectRole)
import UnisonShare.User as User exposing (UserSummaryWithId)


type alias ProjectCollaborator =
    { roles : List ProjectRole
    , user : UserSummaryWithId
    }


decode : Decode.Decoder ProjectCollaborator
decode =
    Decode.succeed ProjectCollaborator
        |> required "roles" (Decode.list ProjectRole.decode)
        |> requiredAt [ "subject", "data" ] User.decodeSummaryWithId
