module UnisonShare.OrgMember exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (failInvalid)
import UnisonShare.OrgRole as OrgRole exposing (OrgRole)
import UnisonShare.User as User exposing (UserSummaryWithId)


type OrgMember
    = UserMember { roles : List OrgRole, user : UserSummaryWithId }
    | TeamMember { roles : List OrgRole, teamId : String }


decodeOrgUserMember : Decode.Decoder OrgMember
decodeOrgUserMember =
    let
        make roles user =
            UserMember { roles = roles, user = user }
    in
    Decode.succeed make
        |> required "roles" (Decode.list OrgRole.decode)
        |> requiredAt [ "subject", "data" ] User.decodeSummaryWithId


decodeOrgTeamMember : Decode.Decoder OrgMember
decodeOrgTeamMember =
    let
        make roles teamId =
            TeamMember { roles = roles, teamId = teamId }
    in
    Decode.succeed make
        |> required "roles" (Decode.list OrgRole.decode)
        |> requiredAt [ "subject", "data", "teamId" ] Decode.string


whenPathIs : List String -> String -> Decode.Decoder a -> Decode.Decoder a
whenPathIs path val =
    when (Decode.at path string) ((==) val)


decodeMaybe : Decode.Decoder (Maybe OrgMember)
decodeMaybe =
    let
        whenKindIs kind =
            whenPathIs [ "subject", "kind" ] kind
    in
    Decode.oneOf
        [ whenKindIs "user" (Decode.map Just decodeOrgUserMember)
        , whenKindIs "team" (Decode.map Just decodeOrgTeamMember)
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
