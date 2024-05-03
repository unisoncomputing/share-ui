module UnisonShare.Project.ProjectRefTests exposing (..)

import Code.ProjectSlug as ProjectSlug
import Expect
import Lib.UserHandle as UserHandle
import Test exposing (..)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


toString : Test
toString =
    describe "ProjectRef.toString"
        [ test "Formats the ref to a string" <|
            \_ ->
                Expect.equal
                    "@unison/http"
                    (ProjectRef.toString projectRef)
        ]


fromString : Test
fromString =
    describe "ProjectRef.fromString"
        [ test "creates a ProjectRef from valid handle string and a valid slug string" <|
            \_ ->
                let
                    result =
                        ProjectRef.fromString "@unison" "http"
                            |> Maybe.map ProjectRef.toString
                            |> Maybe.withDefault "FAIL"
                in
                Expect.equal "@unison/http" result
        ]


unsafeFromString : Test
unsafeFromString =
    describe "ProjectRef.unsafeFromString"
        [ test "creates a ProjectRef from valid unprefixed handle string and a valid slug string" <|
            \_ ->
                let
                    result =
                        ProjectRef.unsafeFromString "unison" "http"
                            |> ProjectRef.toString
                in
                Expect.equal "@unison/http" result
        ]


handle : Test
handle =
    describe "ProjectRef.handle"
        [ test "Extracts the handle" <|
            \_ ->
                Expect.equal
                    "@unison"
                    (ProjectRef.handle projectRef |> UserHandle.toString)
        ]


slug : Test
slug =
    describe "ProjectRef.slug"
        [ test "Extracts the slug" <|
            \_ ->
                Expect.equal
                    "http"
                    (ProjectRef.slug projectRef |> ProjectSlug.toString)
        ]



-- Helpers


projectRef : ProjectRef
projectRef =
    let
        handle_ =
            UserHandle.unsafeFromString "unison"

        slug_ =
            ProjectSlug.unsafeFromString "http"
    in
    ProjectRef.projectRef handle_ slug_
