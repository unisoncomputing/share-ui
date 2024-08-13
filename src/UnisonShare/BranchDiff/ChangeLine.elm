module UnisonShare.BranchDiff.ChangeLine exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder, field, oneOf)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (requiredAt)
import Lib.Util exposing (decodeNonEmptyList, decodeTag)
import List.Nonempty as NEL
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId exposing (ChangeLineId)
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType(..))


type alias NamespaceLineItem =
    { name : FQN, lines : List ChangeLine }


type ChangeLine
    = Added
        DefinitionType
        { hash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        }
    | Removed
        DefinitionType
        { hash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        }
    | Updated
        DefinitionType
        { oldHash : Hash
        , newHash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        }
    | RenamedFrom
        DefinitionType
        { hash : Hash
        , oldNames : NEL.Nonempty FQN
        , newShortName : FQN
        , newFullName : FQN
        , newRef : Reference
        , oldRef : Reference
        }
    | Aliased
        DefinitionType
        { hash : Hash
        , aliasShortName : FQN
        , aliasFullName : FQN
        , otherNames : NEL.Nonempty FQN
        , ref : Reference
        }
    | Namespace NamespaceLineItem


definitionType : ChangeLine -> Maybe DefinitionType
definitionType changeLine =
    case changeLine of
        Added dt _ ->
            Just dt

        Removed dt _ ->
            Just dt

        Updated dt _ ->
            Just dt

        RenamedFrom dt _ ->
            Just dt

        Aliased dt _ ->
            Just dt

        Namespace _ ->
            Nothing


fullName : ChangeLine -> FQN
fullName changeLine =
    case changeLine of
        Added _ d ->
            d.fullName

        Removed _ d ->
            d.fullName

        Updated _ d ->
            d.fullName

        RenamedFrom _ d ->
            d.newFullName

        Aliased _ d ->
            d.aliasFullName

        Namespace d ->
            d.name


shortName : ChangeLine -> FQN
shortName changeLine =
    case changeLine of
        Added _ d ->
            d.shortName

        Removed _ d ->
            d.shortName

        Updated _ d ->
            d.shortName

        RenamedFrom _ d ->
            d.newShortName

        Aliased _ d ->
            d.aliasShortName

        Namespace d ->
            d.name


reference : ChangeLine -> Maybe Reference
reference changeLine =
    case changeLine of
        Added _ d ->
            Just d.ref

        Removed _ d ->
            Just d.ref

        Updated _ d ->
            Just d.ref

        RenamedFrom _ d ->
            Just d.newRef

        Aliased _ d ->
            Just d.ref

        Namespace _ ->
            Nothing


toString : ChangeLine -> String
toString line =
    case line of
        Added _ _ ->
            "Added"

        Removed _ _ ->
            "Removed"

        Updated _ _ ->
            "Updated"

        RenamedFrom _ _ ->
            "Renamed"

        Aliased _ _ ->
            "Aliased"

        Namespace _ ->
            "Namespace"


toChangeLineId : ChangeLine -> Maybe ChangeLineId
toChangeLineId line =
    case line of
        Added dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.Added dt d.fullName)

        Removed dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.Removed dt d.fullName)

        Updated dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.Updated dt d.fullName)

        RenamedFrom dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.RenamedFrom dt d.newFullName)

        Aliased dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.Aliased dt d.aliasFullName)

        Namespace _ ->
            Nothing


isNamespace : ChangeLine -> Bool
isNamespace changeLine =
    case changeLine of
        Namespace _ ->
            True

        _ ->
            False



-- DECODE


decodeNamespace : Decoder ChangeLine
decodeNamespace =
    let
        makeNamespace name changes children =
            Namespace { name = name, lines = changes ++ children }
    in
    Decode.succeed
        makeNamespace
        |> requiredAt [ "path" ] FQN.decode
        |> requiredAt [ "contents", "changes" ] (Decode.list decode)
        |> requiredAt [ "contents", "children" ] (Decode.list (Decode.lazy (\_ -> decodeNamespace)))


decode_ : DefinitionType -> Decoder ChangeLine
decode_ type_ =
    let
        added_ hash shortName_ fullName_ =
            Added type_
                { hash = hash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                }

        removed_ hash shortName_ fullName_ =
            Removed type_
                { hash = hash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                }

        updated_ oldHash newHash shortName_ fullName_ =
            Updated type_
                { oldHash = oldHash
                , newHash = newHash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                }

        renamedFrom_ hash oldNames newShortName newFullName =
            RenamedFrom type_
                { hash = hash
                , oldNames = oldNames
                , newShortName = newShortName
                , newFullName = newFullName
                , oldRef = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) (NEL.head oldNames)
                , newRef = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) newFullName
                }

        aliased_ hash aliasShortName aliasFullName otherNames =
            Aliased type_
                { hash = hash
                , aliasShortName = aliasShortName
                , otherNames = otherNames
                , aliasFullName = aliasFullName
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) aliasFullName
                }
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


decode : Decoder ChangeLine
decode =
    oneOf
        [ when decodeTag ((==) "Plain") (field "contents" (decode_ Term))
        , when decodeTag ((==) "Data") (field "contents" (decode_ Type))
        , when decodeTag ((==) "Ability") (field "contents" (decode_ Ability))
        , when decodeTag ((==) "Doc") (field "contents" (decode_ Doc))
        , when decodeTag ((==) "Test") (field "contents" (decode_ Test))
        , when decodeTag ((==) "DataConstructor") (field "contents" (decode_ DataConstructor))
        , when decodeTag ((==) "AbilityConstructor") (field "contents" (decode_ AbilityConstructor))
        ]
