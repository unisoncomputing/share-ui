module UnisonShare.BranchDiffTests exposing (..)

import Code.BranchRef as BranchRef
import Code.Definition.Reference as Reference
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.Syntax.SyntaxSegment as SyntaxSegment
import Expect
import List.Nonempty as NEL
import Test exposing (..)
import UnisonShare.BranchDiff as BranchDiff
import UnisonShare.BranchDiff.ChangeLine as ChangeLine
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId
import UnisonShare.BranchDiff.DefinitionType as DefinitionType
import UnisonShare.DefinitionDiff as DefinitionDiff


updateChangeLineById : Test
updateChangeLineById =
    describe "BranchDiff.updateChangeLineById"
        [ test "calls the updater function if the change line matches" <|
            \_ ->
                let
                    fullName =
                        FQN.fromString "data.List.map"

                    updatedChangeLine =
                        changeLine fullName (FQN.fromString "List.map")

                    changeLineId =
                        ChangeLineId.changeLineId
                            ChangeLineId.Updated
                            DefinitionType.Term
                            fullName

                    update_ cl =
                        case cl of
                            ChangeLine.Updated dt d ->
                                ChangeLine.Updated dt { d | shortName = FQN.fromString "Updated" }

                            _ ->
                                cl

                    input =
                        branchDiff [ updatedChangeLine ]

                    expected =
                        branchDiff [ changeLine fullName (FQN.fromString "Updated") ]

                    result =
                        BranchDiff.updateChangeLineById update_ changeLineId input
                in
                Expect.equal expected result
        , test "calls the updater function if the change line matches even if that change line is nested inside of Namespace" <|
            \_ ->
                let
                    fullName =
                        FQN.fromString "data.List.map"

                    updatedChangeLine =
                        changeLine fullName (FQN.fromString "List.map")

                    namespace =
                        ChangeLine.Namespace
                            { name = FQN.fromString "data.List"
                            , lines = [ updatedChangeLine ]
                            }

                    changeLineId =
                        ChangeLineId.changeLineId
                            ChangeLineId.Updated
                            DefinitionType.Term
                            fullName

                    update_ cl =
                        case cl of
                            ChangeLine.Updated dt d ->
                                ChangeLine.Updated dt { d | shortName = FQN.fromString "Updated" }

                            _ ->
                                cl

                    input =
                        branchDiff [ namespace ]

                    expected =
                        branchDiff
                            [ ChangeLine.Namespace
                                { name = FQN.fromString "data.List"
                                , lines = [ changeLine fullName (FQN.fromString "Updated") ]
                                }
                            ]

                    result =
                        BranchDiff.updateChangeLineById update_ changeLineId input
                in
                Expect.equal expected result
        ]


branchDiff : List ChangeLine.ChangeLine -> BranchDiff.BranchDiff
branchDiff lines =
    { lines = lines
    , oldBranch = oldBranch
    , newBranch = newBranch
    , libDeps = []
    }


oldBranch : BranchDiff.DiffBranchRef
oldBranch =
    { ref = BranchRef.unsafeFromString "@unison/oldBranch"
    , hash = Hash.unsafeFromString "oldBranch"
    }


newBranch : BranchDiff.DiffBranchRef
newBranch =
    { ref = BranchRef.unsafeFromString "@unison/newBranch"
    , hash = Hash.unsafeFromString "newbranch"
    }


changeLine : FQN.FQN -> FQN.FQN -> ChangeLine.ChangeLine
changeLine fullName shortName =
    ChangeLine.Updated
        DefinitionType.Term
        { oldHash = Hash.unsafeFromString "#oldHash"
        , newHash = Hash.unsafeFromString "#newHash"
        , shortName = shortName
        , fullName = fullName
        , ref = Reference.fromFQN Reference.TermReference fullName
        , diff = diff
        }


diff : DefinitionDiff.DefinitionDiff
diff =
    let
        diffDetails =
            { type_ = DefinitionDiff.Term
            , left =
                [ DefinitionDiff.NotCollapsed
                    [ DefinitionDiff.ChangedLine
                        { lineNum = 1
                        , segments =
                            [ DefinitionDiff.Both (NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral "oldDef"))
                            ]
                        }
                    ]
                ]
            , right =
                [ DefinitionDiff.NotCollapsed
                    [ DefinitionDiff.ChangedLine
                        { lineNum = 1
                        , segments =
                            [ DefinitionDiff.Both (NEL.singleton (SyntaxSegment.SyntaxSegment SyntaxSegment.TextLiteral "newDef"))
                            ]
                        }
                    ]
                ]
            }
    in
    DefinitionDiff.Diff diffDetails
