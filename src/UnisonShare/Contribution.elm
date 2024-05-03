module UnisonShare.Contribution exposing
    ( Contribution
    , dateOfHistoricDiffSupport
    , decode
    )

import Code.BranchRef as BranchRef exposing (BranchRef)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type alias Contribution =
    { ref : ContributionRef
    , authorHandle : Maybe UserHandle
    , sourceBranchRef : BranchRef
    , targetBranchRef : BranchRef
    , projectRef : ProjectRef
    , createdAt : DateTime
    , updatedAt : DateTime
    , status : ContributionStatus
    , numComments : Int
    , title : String
    , description : Maybe String
    }



-- HELPERS


dateOfHistoricDiffSupport : DateTime
dateOfHistoricDiffSupport =
    DateTime.unsafeFromISO8601 "2024-04-30T00:00:00.001Z"



-- DECODE


decode : Decode.Decoder Contribution
decode =
    Decode.succeed Contribution
        |> required "number" ContributionRef.decode
        |> optional "author" (Decode.map Just UserHandle.decodeUnprefixed) Nothing
        |> required "sourceBranchRef" BranchRef.decode
        |> required "targetBranchRef" BranchRef.decode
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" ContributionStatus.decode
        |> required "numComments" Decode.int
        |> required "title" Decode.string
        |> optional "description" (Decode.map Just Decode.string) Nothing
