module UnisonShare.BranchDiff.ChangeLineTests exposing (..)

import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Syntax as Syntax
import Code.Syntax.SyntaxSegment as SyntaxSegment
import Expect
import List.Nonempty as NEL
import Maybe.Extra as MaybeE
import Test exposing (..)
import UnisonShare.BranchDiff.ChangeLine as ChangeLine
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId
import UnisonShare.BranchDiff.DefinitionType as DefinitionType
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff)


equals : Test
equals =
    describe "ChangeLine.byId"
        [ test "Returns immediately when matching the top level ChangeLine" <|
            \_ ->
                let
                    fullName_ =
                        FQN.fromString "data.List.map"

                    cl =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.map"
                            , fullName = fullName_
                            , ref =
                                Reference.fromFQN
                                    Reference.TermReference
                                    fullName_
                            , source = source
                            }

                    id =
                        ChangeLineId.changeLineId
                            ChangeLineId.Added
                            DefinitionType.Term
                            fullName_
                in
                Expect.equal
                    (Just cl)
                    (ChangeLine.byId id cl)
        , test "Finds a nested ChangeLine" <|
            \_ ->
                let
                    fullName_ =
                        FQN.fromString "data.List.map"

                    cl =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.map"
                            , fullName = fullName_
                            , ref =
                                Reference.fromFQN
                                    Reference.TermReference
                                    fullName_
                            , source = source
                            }

                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List", lines = [ cl ] }

                    id =
                        ChangeLineId.changeLineId
                            ChangeLineId.Added
                            DefinitionType.Term
                            fullName_
                in
                Expect.equal
                    (Just cl)
                    (ChangeLine.byId id namespace)
        , test "Returns Nothing when no ChangeLine matches" <|
            \_ ->
                let
                    fullName1 =
                        FQN.fromString "data.List.foldLeft"

                    fullName2 =
                        FQN.fromString "data.List.map"

                    cl =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.foldLeft"
                            , fullName = fullName1
                            , ref =
                                Reference.fromFQN
                                    Reference.TermReference
                                    fullName1
                            , source = source
                            }

                    id =
                        ChangeLineId.changeLineId
                            ChangeLineId.Added
                            DefinitionType.Term
                            fullName2
                in
                Expect.equal
                    Nothing
                    (ChangeLine.byId id cl)
        ]


fullName : Test
fullName =
    describe "ChangeLine.fullName"
        [ test "Returns the appropriate name for the ChangeLine type" <|
            \_ ->
                let
                    added =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.added"
                            , fullName = FQN.fromString "data.List.added"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.added")
                            , source = source
                            }

                    removed =
                        ChangeLine.Removed
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.removed"
                            , fullName = FQN.fromString "data.List.removed"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.removed")
                            , source = source
                            }

                    updated =
                        ChangeLine.Updated
                            DefinitionType.Term
                            { oldHash = Hash.unsafeFromString "#hash"
                            , newHash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.updated"
                            , fullName = FQN.fromString "data.List.updated"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.updated")
                            , diff = diff
                            }

                    renamedFrom =
                        ChangeLine.RenamedFrom
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , oldNames = NEL.singleton (FQN.fromString "List.renamedFromOld")
                            , newShortName = FQN.fromString "List.renamedFrom"
                            , newFullName = FQN.fromString "data.List.renamedFrom"
                            , oldRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFromOld")
                            , newRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFrom")
                            , source = source
                            }

                    aliased =
                        ChangeLine.Aliased
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , aliasShortName = FQN.fromString "List.aliased"
                            , aliasFullName = FQN.fromString "data.List.aliased"
                            , otherNames = NEL.singleton (FQN.fromString "List.aliasedOther")
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.aliased")
                            , source = source
                            }

                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List.namespace"
                            , lines = []
                            }
                in
                Expect.equal
                    [ "data.List.added", "data.List.removed", "data.List.updated", "data.List.renamedFrom", "data.List.aliased", "data.List.namespace" ]
                    ([ added, removed, updated, renamedFrom, aliased, namespace ] |> List.map (ChangeLine.fullName >> FQN.toString))
        ]


shortName : Test
shortName =
    describe "ChangeLine.shortName"
        [ test "Returns the appropriate name for the ChangeLine type" <|
            \_ ->
                let
                    added =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.added"
                            , fullName = FQN.fromString "data.List.added"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.added")
                            , source = source
                            }

                    removed =
                        ChangeLine.Removed
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.removed"
                            , fullName = FQN.fromString "data.List.removed"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.removed")
                            , source = source
                            }

                    updated =
                        ChangeLine.Updated
                            DefinitionType.Term
                            { oldHash = Hash.unsafeFromString "#hash"
                            , newHash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.updated"
                            , fullName = FQN.fromString "data.List.updated"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.updated")
                            , diff = diff
                            }

                    renamedFrom =
                        ChangeLine.RenamedFrom
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , oldNames = NEL.singleton (FQN.fromString "List.renamedFromOld")
                            , newShortName = FQN.fromString "List.renamedFrom"
                            , newFullName = FQN.fromString "data.List.renamedFrom"
                            , oldRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFromOld")
                            , newRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFrom")
                            , source = source
                            }

                    aliased =
                        ChangeLine.Aliased
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , aliasShortName = FQN.fromString "List.aliased"
                            , aliasFullName = FQN.fromString "data.List.aliased"
                            , otherNames = NEL.singleton (FQN.fromString "List.aliasedOther")
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.aliased")
                            , source = source
                            }

                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List.namespace"
                            , lines = []
                            }
                in
                Expect.equal
                    [ "List.added", "List.removed", "List.updated", "List.renamedFrom", "List.aliased", "data.List.namespace" ]
                    ([ added, removed, updated, renamedFrom, aliased, namespace ] |> List.map (ChangeLine.shortName >> FQN.toString))
        ]


reference : Test
reference =
    describe "ChangeLine.reference"
        [ test "Returns the appropriate reference for the ChangeLine type" <|
            \_ ->
                let
                    added =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.added"
                            , fullName = FQN.fromString "data.List.added"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.added")
                            , source = source
                            }

                    removed =
                        ChangeLine.Removed
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.removed"
                            , fullName = FQN.fromString "data.List.removed"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.removed")
                            , source = source
                            }

                    updated =
                        ChangeLine.Updated
                            DefinitionType.Term
                            { oldHash = Hash.unsafeFromString "#hash"
                            , newHash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.updated"
                            , fullName = FQN.fromString "data.List.updated"
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.updated")
                            , diff = diff
                            }

                    renamedFrom =
                        ChangeLine.RenamedFrom
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , oldNames = NEL.singleton (FQN.fromString "List.renamedFromOld")
                            , newShortName = FQN.fromString "List.renamedFrom"
                            , newFullName = FQN.fromString "data.List.renamedFrom"
                            , oldRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFromOld")
                            , newRef = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.renamedFrom")
                            , source = source
                            }

                    aliased =
                        ChangeLine.Aliased
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , aliasShortName = FQN.fromString "List.aliased"
                            , aliasFullName = FQN.fromString "data.List.aliased"
                            , otherNames = NEL.singleton (FQN.fromString "List.aliasedOther")
                            , ref = Reference.fromFQN Reference.TermReference (FQN.fromString "data.List.aliased")
                            , source = source
                            }

                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List.namespace"
                            , lines = []
                            }
                in
                Expect.equal
                    [ "term__data.List.added", "term__data.List.removed", "term__data.List.updated", "term__data.List.renamedFrom", "term__data.List.aliased" ]
                    ([ added, removed, updated, renamedFrom, aliased, namespace ] |> List.map ChangeLine.reference |> List.map (Maybe.map Reference.toString) |> MaybeE.values)
        ]


toChangeLineId : Test
toChangeLineId =
    describe "ChangeLine.toChangeLineId"
        [ test "Creates a ChangeLineId based on the changeline type, definition type, and fullName" <|
            \_ ->
                let
                    fullName_ =
                        FQN.fromString "data.List.map"

                    id =
                        ChangeLineId.changeLineId ChangeLineId.Added DefinitionType.Term fullName_

                    cl =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.map"
                            , fullName = fullName_
                            , ref =
                                Reference.fromFQN
                                    Reference.TermReference
                                    fullName_
                            , source = source
                            }
                in
                Expect.equal
                    True
                    (cl
                        |> ChangeLine.toChangeLineId
                        |> Maybe.map (ChangeLineId.equals id)
                        |> Maybe.withDefault False
                    )
        ]


isNamespace : Test
isNamespace =
    describe "ChangeLine.isNamespace"
        [ test "Returns True when the ChangeLine is a Namespace" <|
            \_ ->
                let
                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List", lines = [] }
                in
                Expect.equal
                    True
                    (ChangeLine.isNamespace namespace)
        , test "Returns False when the ChangeLine is not a Namespace" <|
            \_ ->
                let
                    fullName_ =
                        FQN.fromString "data.List.map"

                    cl =
                        ChangeLine.Added
                            DefinitionType.Term
                            { hash = Hash.unsafeFromString "#hash"
                            , shortName = FQN.fromString "List.map"
                            , fullName = fullName_
                            , ref =
                                Reference.fromFQN
                                    Reference.TermReference
                                    fullName_
                            , source = source
                            }
                in
                Expect.equal
                    False
                    (ChangeLine.isNamespace cl)
        ]


source : Syntax.Syntax
source =
    Syntax.fromNEL
        (NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral ""))


diff : DefinitionDiff
diff =
    let
        diffDetails =
            { type_ = DefinitionDiff.Term
            , newDef = NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral "newDef")
            , oldDef = NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral "oldDef")
            }
    in
    DefinitionDiff.Diff
        diffDetails
        (NEL.singleton (DefinitionDiff.Both (NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral "both"))))
