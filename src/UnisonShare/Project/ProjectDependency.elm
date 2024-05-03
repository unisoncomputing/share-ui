module UnisonShare.Project.ProjectDependency exposing (ProjectDependency, fromString, toTag)

import Code.Version as Version exposing (Version)
import Maybe.Extra as MaybeE
import UI.Tag as Tag exposing (Tag)


type alias ProjectDependency =
    { name : String, version : Maybe Version }


fromString : String -> ProjectDependency
fromString nameWithVersion =
    let
        ( name, version ) =
            case String.split "_" nameWithVersion of
                n :: v ->
                    ( n, Version.fromString (String.join "." v) )

                _ ->
                    ( nameWithVersion, Nothing )
    in
    ProjectDependency name version


toTag : ProjectDependency -> Tag msg
toTag { name, version } =
    name
        |> Tag.tag
        |> Tag.large
        |> Tag.withRightText (MaybeE.unwrap "" (\v -> "@" ++ Version.toString v) version)
