module UnisonShare.CodeBrowsingContext exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type alias CodeBrowsingContext =
    { projectRef : ProjectRef, branchRef : BranchRef }


equals : CodeBrowsingContext -> CodeBrowsingContext -> Bool
equals contextA contextB =
    ProjectRef.equals contextA.projectRef contextB.projectRef && BranchRef.equals contextA.branchRef contextB.branchRef
