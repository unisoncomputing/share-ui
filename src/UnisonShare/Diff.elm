module UnisonShare.Diff exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder, field, oneOf)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Util exposing (decodeNonEmptyList, decodeTag)
import List.Nonempty as NEL


type DefinitionDiff
    = Added { hash : Hash, shortName : FQN, fullName : FQN }
    | Removed { hash : Hash, shortName : FQN, fullName : FQN }
    | Updated { oldHash : Hash, newHash : Hash, shortName : FQN, fullName : FQN }
    | RenamedFrom { hash : Hash, oldNames : NEL.Nonempty FQN, newShortName : FQN, newFullName : FQN }
    | Aliased { hash : Hash, aliasShortName : FQN, aliasFullName : FQN, otherNames : NEL.Nonempty FQN }


type DiffLine
    = TermDiffLine DefinitionDiff
    | TypeDiffLine DefinitionDiff
    | AbilityDiffLine DefinitionDiff
    | DocDiffLine DefinitionDiff
    | TestDiffLine DefinitionDiff
    | DataConstructorDiffLine DefinitionDiff
    | AbilityConstructorDiffLine DefinitionDiff
    | NamespaceDiffLine { name : FQN, lines : List DiffLine }


type alias DiffBranchRef =
    { ref : BranchRef, hash : Hash }


type alias Diff =
    { lines : List DiffLine
    , oldBranch : DiffBranchRef
    , newBranch : DiffBranchRef
    }



-- HELPERS


summary : List DiffLine -> { numChanges : Int, numNamespaceChanges : Int }
summary diffLines =
    let
        f diffLine acc =
            case diffLine of
                NamespaceDiffLine { lines } ->
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
    List.foldl f { numChanges = 0, numNamespaceChanges = 0 } diffLines


sortDiffLines : List DiffLine -> List DiffLine
sortDiffLines lines =
    let
        sortKey diffLine =
            let
                name_ defDiff =
                    case defDiff of
                        Added { shortName } ->
                            shortName

                        Removed { shortName } ->
                            shortName

                        Updated { shortName } ->
                            shortName

                        RenamedFrom { newShortName } ->
                            newShortName

                        Aliased { aliasShortName } ->
                            aliasShortName
            in
            case diffLine of
                TermDiffLine defDiff ->
                    name_ defDiff

                TypeDiffLine defDiff ->
                    name_ defDiff

                AbilityDiffLine defDiff ->
                    name_ defDiff

                DocDiffLine defDiff ->
                    name_ defDiff

                TestDiffLine defDiff ->
                    name_ defDiff

                DataConstructorDiffLine defDiff ->
                    name_ defDiff

                AbilityConstructorDiffLine defDiff ->
                    name_ defDiff

                NamespaceDiffLine ns ->
                    ns.name
    in
    List.sortBy (sortKey >> FQN.toString) lines



-- DECODE


decodeDefinitionDiff : Decoder DefinitionDiff
decodeDefinitionDiff =
    let
        added_ hash shortName fullName =
            Added { hash = hash, shortName = shortName, fullName = fullName }

        removed_ hash shortName fullName =
            Removed { hash = hash, shortName = shortName, fullName = fullName }

        updated_ oldHash newHash shortName fullName =
            Updated { oldHash = oldHash, newHash = newHash, shortName = shortName, fullName = fullName }

        renamedFrom_ hash oldNames newShortName newFullName =
            RenamedFrom { hash = hash, oldNames = oldNames, newShortName = newShortName, newFullName = newFullName }

        aliased_ hash aliasShortName aliasFullName otherNames =
            Aliased { hash = hash, aliasShortName = aliasShortName, otherNames = otherNames, aliasFullName = aliasFullName }
    in
    oneOf
        [ when decodeTag
            ((==) "Added")
            (Decode.succeed added_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
            )
        , when decodeTag
            ((==) "Removed")
            (Decode.succeed removed_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
            )
        , when decodeTag
            ((==) "Updated")
            (Decode.succeed updated_
                |> requiredAt [ "contents", "oldHash" ] Hash.decode
                |> requiredAt [ "contents", "newHash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
            )
        , when decodeTag
            ((==) "RenamedFrom")
            (Decode.succeed renamedFrom_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "oldNames" ] (decodeNonEmptyList FQN.decode)
                |> requiredAt [ "contents", "newShortName" ] FQN.decode
                |> requiredAt [ "contents", "newFullName" ] FQN.decode
            )
        , when decodeTag
            ((==) "Aliased")
            (Decode.succeed aliased_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "aliasShortName" ] FQN.decode
                |> requiredAt [ "contents", "aliasFullName" ] FQN.decode
                |> requiredAt [ "contents", "otherNames" ] (decodeNonEmptyList FQN.decode)
            )
        ]


decodeDiffLine : Decoder DiffLine
decodeDiffLine =
    oneOf
        [ when decodeTag ((==) "Plain") (Decode.map TermDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "Data") (Decode.map TypeDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "Ability") (Decode.map AbilityDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "Doc") (Decode.map DocDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "Test") (Decode.map TestDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "DataConstructor") (Decode.map DataConstructorDiffLine (field "contents" decodeDefinitionDiff))
        , when decodeTag ((==) "AbilityConstructor") (Decode.map AbilityConstructorDiffLine (field "contents" decodeDefinitionDiff))
        ]


decodeNamespace : Decoder DiffLine
decodeNamespace =
    let
        makeNamespace name changes children =
            NamespaceDiffLine { name = name, lines = changes ++ children }
    in
    Decode.succeed
        makeNamespace
        |> requiredAt [ "path" ] FQN.decode
        |> requiredAt [ "contents", "changes" ] (Decode.list decodeDiffLine)
        |> requiredAt [ "contents", "children" ] (Decode.list (Decode.lazy (\_ -> decodeNamespace)))


decode : Decoder Diff
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
        |> requiredAt [ "diff", "changes" ] (Decode.list decodeDiffLine)
        |> requiredAt [ "diff", "children" ] (Decode.list decodeNamespace)
