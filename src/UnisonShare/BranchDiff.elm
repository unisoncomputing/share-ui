module UnisonShare.BranchDiff exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import List.Extra as ListE
import Maybe.Extra as MaybeE
import UnisonShare.BranchDiff.ChangeLine as ChangeLine exposing (ChangeLine(..))
import UnisonShare.BranchDiff.ChangeLineId exposing (ChangeLineId)


type alias DiffBranchRef =
    { ref : BranchRef, hash : Hash }


type alias BranchDiff =
    { lines : List ChangeLine
    , oldBranch : DiffBranchRef
    , newBranch : DiffBranchRef
    }



-- HELPERS


size : BranchDiff -> Int
size branchDiff =
    branchDiff |> summary |> .numChanges


summary : BranchDiff -> { numChanges : Int, numNamespaceChanges : Int }
summary branchDiff =
    let
        go lines =
            List.foldl f { numChanges = 0, numNamespaceChanges = 0 } lines

        f changeLine acc =
            case changeLine of
                Namespace { lines } ->
                    let
                        nested =
                            go lines

                        -- This matters because we ignore Propagated, which can be nested quite deep.
                        countNamespaceAsInclusiveOfChanges =
                            if acc.numChanges == nested.numChanges then
                                0

                            else
                                1
                    in
                    { numChanges = acc.numChanges + nested.numChanges
                    , numNamespaceChanges = acc.numNamespaceChanges + nested.numNamespaceChanges + countNamespaceAsInclusiveOfChanges
                    }

                Propagated _ _ ->
                    acc

                _ ->
                    { acc | numChanges = acc.numChanges + 1 }
    in
    go branchDiff.lines


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

                Propagated type_ ({ shortName } as details) ->
                    Propagated type_ { details | shortName = FQN.append ns shortName }

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


changeLineById : ChangeLineId -> BranchDiff -> Maybe ChangeLine
changeLineById changeLineId branchDiff =
    let
        f cl_ acc =
            if MaybeE.isJust acc then
                acc

            else
                ChangeLine.byId changeLineId cl_
    in
    List.foldl f Nothing branchDiff.lines



-- DECODE


decode : Decoder BranchDiff
decode =
    let
        mk oldRef oldRefHash newRef newRefHash changes children =
            { oldBranch = { ref = oldRef, hash = oldRefHash }
            , newBranch = { ref = newRef, hash = newRefHash }
            , lines = changes ++ children
            }

        {- TODO backwards compat to support the new nesting with `defns` to be deployed before backend is ready -}
        changeLines =
            Decode.oneOf
                [ Decode.at [ "defns", "changes" ] (Decode.list ChangeLine.decode)
                , Decode.field "changes" (Decode.list ChangeLine.decode)
                ]

        namespaces =
            Decode.oneOf
                [ Decode.at [ "defns", "children" ] (Decode.list ChangeLine.decodeNamespace)
                , Decode.field "children" (Decode.list ChangeLine.decodeNamespace)
                ]
    in
    Decode.succeed mk
        |> required "oldRef" BranchRef.decode
        |> required "oldRefHash" Hash.decode
        |> required "newRef" BranchRef.decode
        |> required "newRefHash" Hash.decode
        |> required "diff" changeLines
        |> required "diff" namespaces
