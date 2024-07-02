module UnisonShare.DefinitionDiffKey exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.FullyQualifiedName as FQN exposing (FQN)


type alias DefinitionDiffKeyData =
    { branchA : BranchRef
    , branchB : BranchRef
    , definitionA : FQN
    , definitionB : FQN
    }


type DefinitionDiffKey
    = Term DefinitionDiffKeyData
    | Type DefinitionDiffKeyData


equals : DefinitionDiffKey -> DefinitionDiffKey -> Bool
equals a b =
    case ( a, b ) of
        ( Term a_, Term b_ ) ->
            BranchRef.equals a_.branchA b_.branchA
                && BranchRef.equals a_.branchB b_.branchB
                && FQN.equals a_.definitionA a_.definitionB

        ( Type a_, Type b_ ) ->
            BranchRef.equals a_.branchA b_.branchA
                && BranchRef.equals a_.branchB b_.branchB
                && FQN.equals a_.definitionA a_.definitionB

        _ ->
            False


toString : DefinitionDiffKey -> String
toString k =
    let
        toString_ prefix data =
            String.join "_"
                [ prefix
                , BranchRef.toString data.branchA
                , BranchRef.toString data.branchB
                , FQN.toString data.definitionA
                , FQN.toString data.definitionB
                ]
    in
    case k of
        Term d ->
            toString_ "term" d

        Type d ->
            toString_ "type" d
