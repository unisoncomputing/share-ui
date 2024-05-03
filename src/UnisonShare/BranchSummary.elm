module UnisonShare.BranchSummary exposing (BranchSummary, decode)

import Code.Branch as Branch
import Json.Decode as Decode
import UnisonShare.Project as Project exposing (Project)


type alias BranchSummary =
    Branch.BranchSummary (Project {})


decode : Decode.Decoder BranchSummary
decode =
    Branch.decodeSummary Project.decode
