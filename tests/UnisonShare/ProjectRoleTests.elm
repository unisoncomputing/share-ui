module UnisonShare.ProjectRoleTests exposing (..)

import Expect
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import UnisonShare.ProjectRole as ProjectRole exposing (ProjectRole(..))


decode : Test
decode =
    describe "ProjectRole.decode"
        [ test "Can parse roles" <|
            \_ ->
                let
                    result =
                        Decode.decodeString (Decode.list ProjectRole.decode) allRolesJson
                in
                Expect.equal (Ok [ Owner, Admin, Maintainer, Contributor, Viewer ]) result
        ]


encode : Test
encode =
    describe "ProjectRole.encode"
        [ test "Can encode roles" <|
            \_ ->
                let
                    result =
                        [ Owner
                        , Admin
                        , Maintainer
                        , Contributor
                        , Viewer
                        ]
                            |> Encode.list ProjectRole.encode
                            |> Encode.encode 0
                in
                Expect.equal
                    """["project_owner","project_admin","project_maintainer","project_contributor","project_viewer"]"""
                    result
        ]


allRolesJson : String
allRolesJson =
    """
  ["project_owner", "project_admin", "project_maintainer", "project_contributor", "project_viewer"]
  """
