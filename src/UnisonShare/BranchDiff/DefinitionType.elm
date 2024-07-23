module UnisonShare.BranchDiff.DefinitionType exposing (..)


type DefinitionType
    = Term
    | Type
    | Ability
    | Doc
    | Test
    | DataConstructor
    | AbilityConstructor


isTerm : DefinitionType -> Bool
isTerm dt =
    case dt of
        Term ->
            True

        Doc ->
            True

        Test ->
            True

        _ ->
            False


isType : DefinitionType -> Bool
isType dt =
    not (isTerm dt)
