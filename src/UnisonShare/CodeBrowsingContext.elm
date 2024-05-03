module UnisonShare.CodeBrowsingContext exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type CodeBrowsingContext
    = UserCode UserHandle
    | ProjectBranch ProjectRef BranchRef


user : UserHandle -> CodeBrowsingContext
user =
    UserCode


project : ProjectRef -> BranchRef -> CodeBrowsingContext
project =
    ProjectBranch


equals : CodeBrowsingContext -> CodeBrowsingContext -> Bool
equals a b =
    case ( a, b ) of
        ( UserCode handleA, UserCode handleB ) ->
            UserHandle.equals handleA handleB

        ( ProjectBranch projectRefA branchRefA, ProjectBranch projectRefB branchRefB ) ->
            ProjectRef.equals projectRefA projectRefB && BranchRef.equals branchRefA branchRefB

        _ ->
            False
