module UnisonShare.BranchDiff.LibDep exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.ProjectDependency as ProjectDependency exposing (ProjectDependency)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (failInvalid, whenTagIs)
import List.Extra as ListE


type alias LibDepInfo =
    { name : FQN, dep : ProjectDependency }


type LibDep
    = Added LibDepInfo
    | Removed LibDepInfo
    | Updated { before : LibDepInfo, after : LibDepInfo }


same : LibDep -> LibDep -> Bool
same a b =
    ProjectDependency.same (projectDependency a) (projectDependency b)


libDepInfo : LibDep -> LibDepInfo
libDepInfo libDep =
    case libDep of
        Added info ->
            info

        Removed info ->
            info

        Updated { before } ->
            before


projectDependency : LibDep -> ProjectDependency
projectDependency libDep =
    libDep |> libDepInfo |> .dep


mergeUpdated : List LibDep -> List LibDep
mergeUpdated deps =
    let
        go lib acc =
            case ( List.filter (same lib) deps, ListE.find (same lib) acc ) of
                ( [ Added addedInfo, Removed removedInfo ], Nothing ) ->
                    Updated { before = removedInfo, after = addedInfo } :: acc

                ( [ Removed removedInfo, Added addedInfo ], Nothing ) ->
                    Updated { before = removedInfo, after = addedInfo } :: acc

                ( [ Added _, Removed _ ], Just _ ) ->
                    acc

                ( [ Removed _, Added _ ], Just _ ) ->
                    acc

                _ ->
                    lib :: acc
    in
    List.foldr go [] deps



-- DECODE


decodeMaybe : Decode.Decoder (Maybe LibDep)
decodeMaybe =
    let
        makeAdded name =
            Added
                { name = FQN.fromString ("lib." ++ name)
                , dep = ProjectDependency.fromString name
                }

        makeRemoved name =
            Removed
                { name = FQN.fromString ("lib." ++ name)
                , dep = ProjectDependency.fromString name
                }
    in
    Decode.oneOf
        [ whenTagIs "Added"
            (Decode.map Just
                (Decode.succeed makeAdded
                    |> required "name" Decode.string
                )
            )
        , whenTagIs "Removed"
            (Decode.map Just
                (Decode.succeed makeRemoved
                    |> required "name" Decode.string
                )
            )
        , Decode.succeed Nothing
        ]


decode : Decode.Decoder LibDep
decode =
    decodeMaybe
        |> Decode.andThen (failInvalid "Invalid Lib dependency")


decodeList : Decode.Decoder (List LibDep)
decodeList =
    Decode.list decodeMaybe
        |> Decode.map (List.filterMap identity)
