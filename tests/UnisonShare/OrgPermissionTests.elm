module UnisonShare.OrgPermissionTests exposing (..)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import UnisonShare.OrgPermission as OrgPermission exposing (OrgPermission(..))


decodeList : Test
decodeList =
    describe "OrgPermission.decodeList"
        [ test "Can parse known permissions" <|
            \_ ->
                let
                    result =
                        Decode.decodeString OrgPermission.decodeList allPermissionsJson
                in
                Expect.equal (Ok [ OrgView, OrgManage, OrgAdmin, OrgCreateProject, OrgDelete, OrgChangeOwner ]) result
        , test "Unknown permissions are ignored" <|
            \_ ->
                let
                    result =
                        Decode.decodeString OrgPermission.decodeList permissionsWithUnknownsJson
                in
                Expect.equal (Ok [ OrgView, OrgManage ]) result
        ]


allPermissionsJson : String
allPermissionsJson =
    """
  ["org:view", "org:manage", "org:admin", "org:create_project", "org:delete", "org:change_owner"]
  """


permissionsWithUnknownsJson : String
permissionsWithUnknownsJson =
    """
  ["org:view", "org:unknown", "org:unknown2", "org:manage"]
  """
