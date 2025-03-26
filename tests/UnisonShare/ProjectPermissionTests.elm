module UnisonShare.ProjectPermissionTests exposing (..)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import UnisonShare.ProjectPermission as ProjectPermission exposing (ProjectPermission(..))


decodeList : Test
decodeList =
    describe "ProjectPermission.decodeList"
        [ test "Can parse known permissions" <|
            \_ ->
                let
                    result =
                        Decode.decodeString ProjectPermission.decodeList allPermissionsJson
                in
                Expect.equal (Ok [ ProjectView, ProjectContribute, ProjectMaintain, ProjectManage, ProjectDelete ]) result
        , test "Unknown permissions are ignored" <|
            \_ ->
                let
                    result =
                        Decode.decodeString ProjectPermission.decodeList permissionsWithUnknownsJson
                in
                Expect.equal (Ok [ ProjectView, ProjectMaintain ]) result
        ]


allPermissionsJson : String
allPermissionsJson =
    """
  ["project:view", "project:contribute", "project:maintain", "project:manage", "project:delete"]
  """


permissionsWithUnknownsJson : String
permissionsWithUnknownsJson =
    """
  ["project:view", "project:unknown", "project:maintain", "org:manage"]
  """
