module UnisonShare.CodeBrowsingContext exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type CodeBrowsingContext
    = ProjectBranch ProjectRef BranchRef


project : ProjectRef -> BranchRef -> CodeBrowsingContext
project =
    ProjectBranch


equals : CodeBrowsingContext -> CodeBrowsingContext -> Bool
equals (ProjectBranch projectRefA branchRefA) (ProjectBranch projectRefB branchRefB) =
    ProjectRef.equals projectRefA projectRefB && BranchRef.equals branchRefA branchRefB
