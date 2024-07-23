module UnisonShare.BranchDiff exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required, requiredAt)
import List.Extra as ListE
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine(..))


type alias DiffBranchRef =
    { ref : BranchRef, hash : Hash }


type alias BranchDiff =
    { lines : List ChangeLine
    , oldBranch : DiffBranchRef
    , newBranch : DiffBranchRef
    }



-- HELPERS


summary : List ChangeLine -> { numChanges : Int, numNamespaceChanges : Int }
summary changeLines =
    let
        f changeLine acc =
            case changeLine of
                Namespace { lines } ->
                    let
                        nested =
                            summary lines
                    in
                    { numChanges = acc.numChanges + nested.numChanges
                    , numNamespaceChanges = acc.numNamespaceChanges + nested.numNamespaceChanges + 1
                    }

                _ ->
                    { acc | numChanges = acc.numChanges + 1 }
    in
    List.foldl f { numChanges = 0, numNamespaceChanges = 0 } changeLines


condense : List ChangeLine -> List ChangeLine
condense changeLines =
    let
        condense_ ns l =
            case l of
                Added type_ ({ shortName } as details) ->
                    Added type_ { details | shortName = FQN.append ns shortName }

                Removed type_ ({ shortName } as details) ->
                    Removed type_ { details | shortName = FQN.append ns shortName }

                Updated type_ ({ shortName } as details) ->
                    Updated type_ { details | shortName = FQN.append ns shortName }

                RenamedFrom type_ ({ newShortName } as details) ->
                    RenamedFrom type_ { details | newShortName = FQN.append ns newShortName }

                Aliased type_ ({ aliasShortName } as details) ->
                    Aliased type_ { details | aliasShortName = FQN.append ns aliasShortName }

                Namespace { name, lines } ->
                    Namespace
                        { name = FQN.append ns name
                        , lines = condense lines
                        }

        f changeLine acc =
            case changeLine of
                Namespace { name, lines } ->
                    case lines of
                        [ line ] ->
                            let
                                condensedLine =
                                    condense_ name line
                            in
                            if ChangeLine.isNamespace condensedLine then
                                acc ++ [ condensedLine ]

                            else
                                case ListE.splitWhen ChangeLine.isNamespace acc of
                                    Just ( definitions, namespaces ) ->
                                        definitions ++ (condensedLine :: namespaces)

                                    Nothing ->
                                        acc ++ [ condensedLine ]

                        _ ->
                            acc ++ [ Namespace { name = name, lines = condense lines } ]

                _ ->
                    acc ++ [ changeLine ]
    in
    List.foldl f [] changeLines



-- DECODE


decode : Decoder BranchDiff
decode =
    let
        mk oldRef oldRefHash newRef newRefHash changes children =
            { oldBranch = { ref = oldRef, hash = oldRefHash }
            , newBranch = { ref = newRef, hash = newRefHash }
            , lines = changes ++ children
            }
    in
    Decode.succeed mk
        |> required "oldRef" BranchRef.decode
        |> required "oldRefHash" Hash.decode
        |> required "newRef" BranchRef.decode
        |> required "newRefHash" Hash.decode
        |> requiredAt [ "diff", "changes" ] (Decode.list ChangeLine.decode)
        |> requiredAt [ "diff", "children" ] (Decode.list ChangeLine.decodeNamespace)
