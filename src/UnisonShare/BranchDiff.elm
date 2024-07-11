module UnisonShare.BranchDiff exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode exposing (Decoder, field, oneOf)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Util exposing (decodeNonEmptyList, decodeTag)
import List.Extra as ListE
import List.Nonempty as NEL


type alias NamespaceLineItem =
    { name : FQN, lines : List ChangeLine }


type DefinitionType
    = Term
    | Type
    | Ability
    | Doc
    | Test
    | DataConstructor
    | AbilityConstructor


type ChangeLine
    = Added DefinitionType { hash : Hash, shortName : FQN, fullName : FQN }
    | Removed DefinitionType { hash : Hash, shortName : FQN, fullName : FQN }
    | Updated DefinitionType { oldHash : Hash, newHash : Hash, shortName : FQN, fullName : FQN }
    | RenamedFrom DefinitionType { hash : Hash, oldNames : NEL.Nonempty FQN, newShortName : FQN, newFullName : FQN }
    | Aliased DefinitionType { hash : Hash, aliasShortName : FQN, aliasFullName : FQN, otherNames : NEL.Nonempty FQN }
    | Namespace NamespaceLineItem


type alias DiffBranchRef =
    { ref : BranchRef, hash : Hash }


type alias BranchDiff =
    { lines : List ChangeLine
    , oldBranch : DiffBranchRef
    , newBranch : DiffBranchRef
    }



-- HELPERS


isDefinitionTypeATerm : DefinitionType -> Bool
isDefinitionTypeATerm dt =
    case dt of
        Term ->
            True

        Doc ->
            True

        Test ->
            True

        _ ->
            False


isDefinitionTypeAType : DefinitionType -> Bool
isDefinitionTypeAType dt =
    not (isDefinitionTypeATerm dt)


changeLineDefinitionType : ChangeLine -> Maybe DefinitionType
changeLineDefinitionType changeLine =
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


changeLineFullName : ChangeLine -> FQN
changeLineFullName changeLine =
    case changeLine of
        Added _ { fullName } ->
            fullName

        Removed _ { fullName } ->
            fullName

        Updated _ { fullName } ->
            fullName

        RenamedFrom _ { newFullName } ->
            newFullName

        Aliased _ { aliasFullName } ->
            aliasFullName

        Namespace { name } ->
            name


changeLineNameToString : ChangeLine -> String
changeLineNameToString line =
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


changeLineToKey : ChangeLine -> String
changeLineToKey line =
    let
        type_ dt =
            case dt of
                Term ->
                    "term"

                Type ->
                    "type"

                Doc ->
                    "doc"

                Ability ->
                    "ability"

                AbilityConstructor ->
                    "ability-constructor"

                DataConstructor ->
                    "data-constructor"

                Test ->
                    "test"

        key_ =
            case line of
                Added dt { fullName } ->
                    [ "added", type_ dt, FQN.toString fullName ]

                Removed dt { fullName } ->
                    [ "removed", type_ dt, FQN.toString fullName ]

                Updated dt { fullName } ->
                    [ "updated", type_ dt, FQN.toString fullName ]

                RenamedFrom dt { newFullName } ->
                    [ "renamed", type_ dt, FQN.toString newFullName ]

                Aliased dt { aliasFullName } ->
                    [ "aliased", type_ dt, FQN.toString aliasFullName ]

                Namespace { name } ->
                    [ "namespace", FQN.toString name ]
    in
    String.join "_" key_


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


isNamespaceChangeLine : ChangeLine -> Bool
isNamespaceChangeLine changeLine =
    case changeLine of
        Namespace _ ->
            True

        _ ->
            False


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
                            if isNamespaceChangeLine condensedLine then
                                acc ++ [ condensedLine ]

                            else
                                case ListE.splitWhen isNamespaceChangeLine acc of
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


decodeChangeLineItem : DefinitionType -> Decoder ChangeLine
decodeChangeLineItem type_ =
    let
        added_ hash shortName fullName =
            Added type_ { hash = hash, shortName = shortName, fullName = fullName }

        removed_ hash shortName fullName =
            Removed type_ { hash = hash, shortName = shortName, fullName = fullName }

        updated_ oldHash newHash shortName fullName =
            Updated type_ { oldHash = oldHash, newHash = newHash, shortName = shortName, fullName = fullName }

        renamedFrom_ hash oldNames newShortName newFullName =
            RenamedFrom type_ { hash = hash, oldNames = oldNames, newShortName = newShortName, newFullName = newFullName }

        aliased_ hash aliasShortName aliasFullName otherNames =
            Aliased type_ { hash = hash, aliasShortName = aliasShortName, otherNames = otherNames, aliasFullName = aliasFullName }
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


decodeChangeLine : Decoder ChangeLine
decodeChangeLine =
    oneOf
        [ when decodeTag ((==) "Plain") (field "contents" (decodeChangeLineItem Term))
        , when decodeTag ((==) "Data") (field "contents" (decodeChangeLineItem Type))
        , when decodeTag ((==) "Ability") (field "contents" (decodeChangeLineItem Ability))
        , when decodeTag ((==) "Doc") (field "contents" (decodeChangeLineItem Doc))
        , when decodeTag ((==) "Test") (field "contents" (decodeChangeLineItem Test))
        , when decodeTag ((==) "DataConstructor") (field "contents" (decodeChangeLineItem DataConstructor))
        , when decodeTag ((==) "AbilityConstructor") (field "contents" (decodeChangeLineItem AbilityConstructor))
        ]


decodeNamespace : Decoder ChangeLine
decodeNamespace =
    let
        makeNamespace name changes children =
            Namespace { name = name, lines = changes ++ children }
    in
    Decode.succeed
        makeNamespace
        |> requiredAt [ "path" ] FQN.decode
        |> requiredAt [ "contents", "changes" ] (Decode.list decodeChangeLine)
        |> requiredAt [ "contents", "children" ] (Decode.list (Decode.lazy (\_ -> decodeNamespace)))


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
        |> requiredAt [ "diff", "changes" ] (Decode.list decodeChangeLine)
        |> requiredAt [ "diff", "children" ] (Decode.list decodeNamespace)
