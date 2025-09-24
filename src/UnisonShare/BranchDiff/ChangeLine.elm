module UnisonShare.BranchDiff.ChangeLine exposing (..)

import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.Syntax as Syntax exposing (Syntax)
import Json.Decode as Decode exposing (Decoder, field, oneOf)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (requiredAt)
import Lib.Decode.Helpers exposing (nonEmptyList, tag)
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId exposing (ChangeLineId)
import UnisonShare.BranchDiff.DefinitionType as DefinitionType exposing (DefinitionType(..))
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff)


type alias NamespaceLineItem =
    { name : FQN, lines : List ChangeLine }


type ChangeLine
    = Added
        DefinitionType
        { hash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        , source : Syntax
        }
    | Removed
        DefinitionType
        { hash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        , source : Syntax
        }
    | Updated
        DefinitionType
        { oldHash : Hash
        , newHash : Hash
        , shortName : FQN
        , fullName : FQN
        , ref : Reference
        , diff : DefinitionDiff
        }
    | Propagated
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
        , source : Syntax
        }
    | Aliased
        DefinitionType
        { hash : Hash
        , aliasShortName : FQN
        , aliasFullName : FQN
        , otherNames : NEL.Nonempty FQN
        , ref : Reference
        , source : Syntax
        }
    | Namespace NamespaceLineItem


matchesId : ChangeLineId -> ChangeLine -> Bool
matchesId id cl =
    toChangeLineId cl == Just id


isPropagated : ChangeLine -> Bool
isPropagated cl =
    case cl of
        Propagated _ _ ->
            True

        _ ->
            False


shouldBeCollapsedByDefault : ChangeLine -> Bool
shouldBeCollapsedByDefault changeLine =
    case changeLine of
        Aliased _ _ ->
            True

        RenamedFrom _ _ ->
            True

        Removed _ _ ->
            True

        _ ->
            False


byId : ChangeLineId -> ChangeLine -> Maybe ChangeLine
byId id cl =
    case cl of
        Namespace { lines } ->
            let
                f cl_ acc =
                    if MaybeE.isJust acc then
                        acc

                    else
                        byId id cl_
            in
            List.foldl f Nothing lines

        _ ->
            if matchesId id cl then
                Just cl

            else
                Nothing


definitionType : ChangeLine -> Maybe DefinitionType
definitionType changeLine =
    case changeLine of
        Added dt _ ->
            Just dt

        Removed dt _ ->
            Just dt

        Updated dt _ ->
            Just dt

        Propagated dt _ ->
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

        Propagated _ d ->
            d.fullName

        RenamedFrom _ d ->
            d.newFullName

        Aliased _ d ->
            d.aliasFullName

        Namespace d ->
            d.name


source : ChangeLine -> Maybe Syntax
source changeLine =
    case changeLine of
        Added _ d ->
            Just d.source

        Removed _ d ->
            Just d.source

        Updated _ _ ->
            Nothing

        Propagated _ _ ->
            Nothing

        RenamedFrom _ d ->
            Just d.source

        Aliased _ d ->
            Just d.source

        Namespace _ ->
            Nothing


shortName : ChangeLine -> FQN
shortName changeLine =
    case changeLine of
        Added _ d ->
            d.shortName

        Removed _ d ->
            d.shortName

        Updated _ d ->
            d.shortName

        Propagated _ d ->
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

        Propagated _ d ->
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

        Propagated _ _ ->
            "Propagated"

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

        Propagated dt d ->
            Just (ChangeLineId.changeLineId ChangeLineId.Propagated dt d.fullName)

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
        diffType =
            case type_ of
                Term ->
                    DefinitionDiff.Term

                Type ->
                    DefinitionDiff.Type

                Ability ->
                    DefinitionDiff.Type

                Doc ->
                    DefinitionDiff.Term

                Test ->
                    DefinitionDiff.Term

                DataConstructor ->
                    DefinitionDiff.Type

                AbilityConstructor ->
                    DefinitionDiff.Type

        added_ hash shortName_ fullName_ stx =
            Added type_
                { hash = hash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                , source = stx
                }

        removed_ hash shortName_ fullName_ stx =
            Removed type_
                { hash = hash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                , source = stx
                }

        updated_ oldHash newHash shortName_ fullName_ diff =
            Updated type_
                { oldHash = oldHash
                , newHash = newHash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                , diff = diff
                }

        propagated_ oldHash newHash shortName_ fullName_ =
            Propagated type_
                { oldHash = oldHash
                , newHash = newHash
                , shortName = shortName_
                , fullName = fullName_
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) fullName_
                }

        renamedFrom_ hash oldNames newShortName newFullName stx =
            RenamedFrom type_
                { hash = hash
                , oldNames = oldNames
                , newShortName = newShortName
                , newFullName = newFullName
                , oldRef = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) (NEL.head oldNames)
                , newRef = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) newFullName
                , source = stx
                }

        aliased_ hash aliasShortName aliasFullName otherNames stx =
            Aliased type_
                { hash = hash
                , aliasShortName = aliasShortName
                , otherNames = otherNames
                , aliasFullName = aliasFullName
                , ref = Reference.fromFQN (DefinitionType.toReferenceConstructor type_) aliasFullName
                , source = stx
                }

        defSourcePrefix =
            case type_ of
                Type ->
                    "typeDefinition"

                _ ->
                    "termDefinition"
    in
    oneOf
        [ when tag
            ((==) "Added")
            (Decode.succeed added_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
                |> requiredAt [ "contents", "rendered", defSourcePrefix, "contents" ] Syntax.decode
            )
        , when tag
            ((==) "Removed")
            (Decode.succeed removed_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
                |> requiredAt [ "contents", "rendered", defSourcePrefix, "contents" ] Syntax.decode
            )
        , when tag
            ((==) "Updated")
            (Decode.succeed updated_
                |> requiredAt [ "contents", "oldHash" ] Hash.decode
                |> requiredAt [ "contents", "newHash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
                |> requiredAt [ "contents", "diff" ] (DefinitionDiff.decode diffType)
            )
        , when tag
            ((==) "RenamedFrom")
            (Decode.succeed renamedFrom_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "oldNames" ] (nonEmptyList FQN.decode)
                |> requiredAt [ "contents", "newShortName" ] FQN.decode
                |> requiredAt [ "contents", "newFullName" ] FQN.decode
                |> requiredAt [ "contents", "rendered", defSourcePrefix, "contents" ] Syntax.decode
            )
        , when tag
            ((==) "Aliased")
            (Decode.succeed aliased_
                |> requiredAt [ "contents", "hash" ] Hash.decode
                |> requiredAt [ "contents", "aliasShortName" ] FQN.decode
                |> requiredAt [ "contents", "aliasFullName" ] FQN.decode
                |> requiredAt [ "contents", "otherNames" ] (nonEmptyList FQN.decode)
                |> requiredAt [ "contents", "rendered", defSourcePrefix, "contents" ] Syntax.decode
            )
        , when tag
            ((==) "Propagated")
            (Decode.succeed propagated_
                |> requiredAt [ "contents", "oldHash" ] Hash.decode
                |> requiredAt [ "contents", "newHash" ] Hash.decode
                |> requiredAt [ "contents", "shortName" ] FQN.decode
                |> requiredAt [ "contents", "fullName" ] FQN.decode
            )
        ]


decode : Decoder ChangeLine
decode =
    oneOf
        [ when tag ((==) "Plain") (field "contents" (decode_ Term))
        , when tag ((==) "Data") (field "contents" (decode_ Type))
        , when tag ((==) "Ability") (field "contents" (decode_ Ability))
        , when tag ((==) "Doc") (field "contents" (decode_ Doc))
        , when tag ((==) "Test") (field "contents" (decode_ Test))
        , when tag ((==) "DataConstructor") (field "contents" (decode_ DataConstructor))
        , when tag ((==) "AbilityConstructor") (field "contents" (decode_ AbilityConstructor))
        ]
