module UnisonShare.OrgMember exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (failInvalid)
import UnisonShare.OrgRole as OrgRole exposing (OrgRole)
import UnisonShare.User as User exposing (UserSummaryWithId)


type
    OrgMember
    --    | TeamMember { role : OrgRole, teamId : String }
    = UserMember { role : OrgRole, user : UserSummaryWithId }


decodeOrgUserMember : Decode.Decoder OrgMember
decodeOrgUserMember =
    let
        make role user =
            UserMember { role = role, user = user }
    in
    Decode.succeed make
        |> required "role" OrgRole.decode
        |> required "subject" User.decodeSummaryWithId


whenPathIs : List String -> String -> Decode.Decoder a -> Decode.Decoder a
whenPathIs path val =
    when (Decode.at path string) ((==) val)


decodeMaybe : Decode.Decoder (Maybe OrgMember)
decodeMaybe =
    Decode.oneOf
        [ Decode.map Just decodeOrgUserMember
        , Decode.succeed Nothing
        ]


decode : Decode.Decoder OrgMember
decode =
    decodeMaybe
        |> Decode.andThen (failInvalid "Invalid Org Member")


{-| soft fail when encountering unknown member types
-}
decodeList : Decode.Decoder (List OrgMember)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
