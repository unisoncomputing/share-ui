module UnisonShare.Project.ProjectDependency exposing (ProjectDependency, fromString, toString, toTag)

import Code.Version as Version exposing (Version)
import Maybe.Extra as MaybeE
import UI.Tag as Tag exposing (Tag)


type alias ProjectDependency =
    { name : String, version : Maybe Version }


fromString : String -> ProjectDependency
fromString raw =
    let
        parts =
            String.split "_" raw

        ( name, version ) =
            case parts of
                [ user, project, major, minor, patch ] ->
                    ( "@" ++ user ++ "/" ++ project, Version.fromString (String.join "." [ major, minor, patch ]) )

                [ n, major, minor, patch ] ->
                    let
                        version_ =
                            Version.fromString (String.join "." [ major, minor, patch ])
                    in
                    case version_ of
                        Just v ->
                            ( n, Just v )

                        -- It wasn't a version after all, so we give up trying to parse it
                        Nothing ->
                            ( raw, Nothing )

                [ user, project ] ->
                    ( "@" ++ user ++ "/" ++ project, Nothing )

                [ n ] ->
                    ( n, Nothing )

                _ ->
                    case List.reverse parts of
                        patch :: minor :: major :: n ->
                            let
                                version_ =
                                    Version.fromString (String.join "." [ major, minor, patch ])
                            in
                            case version_ of
                                Just v ->
                                    ( String.join "_" (List.reverse n), Just v )

                                -- It wasn't a version after all, so we give up trying to parse it
                                Nothing ->
                                    ( raw, Nothing )

                        _ ->
                            ( raw, Nothing )
    in
    ProjectDependency name version


toString : ProjectDependency -> String
toString { name, version } =
    name ++ MaybeE.unwrap "" (\v -> " v" ++ Version.toString v) version


toTag : ProjectDependency -> Tag msg
toTag { name, version } =
    name
        |> Tag.tag
        |> Tag.large
        |> Tag.withRightText (MaybeE.unwrap "" (\v -> " v" ++ Version.toString v) version)
