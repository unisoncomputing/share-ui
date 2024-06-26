module UnisonShare.Project.ProjectDependencyTests exposing (..)

import Expect
import Test exposing (..)
import UnisonShare.Project.ProjectDependency as ProjectDependency


fromString : Test
fromString =
    describe "ProjectDependency.fromString"
        [ test "parses 'base'" <|
            \_ ->
                Expect.equal
                    "base"
                    (ProjectDependency.toString (ProjectDependency.fromString "base"))
        , test "parses 'unison_base'" <|
            \_ ->
                Expect.equal
                    "@unison/base"
                    (ProjectDependency.toString (ProjectDependency.fromString "unison_base"))
        , test "parses 'unison_base_with_underscore'" <|
            \_ ->
                Expect.equal
                    "unison_base_with_underscore"
                    (ProjectDependency.toString (ProjectDependency.fromString "unison_base_with_underscore"))
        , test "parses 'base_1_2_3'" <|
            \_ ->
                Expect.equal
                    "base v1.2.3"
                    (ProjectDependency.toString (ProjectDependency.fromString "base_1_2_3"))
        , test "parses 'unison_base_1_2_3'" <|
            \_ ->
                Expect.equal
                    "@unison/base v1.2.3"
                    (ProjectDependency.toString (ProjectDependency.fromString "unison_base_1_2_3"))
        , test "parses 'unison_base_with_underscore_1_2_3'" <|
            \_ ->
                Expect.equal
                    "unison_base_with_underscore v1.2.3"
                    (ProjectDependency.toString (ProjectDependency.fromString "unison_base_with_underscore_1_2_3"))
        ]
