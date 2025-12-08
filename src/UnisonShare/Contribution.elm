module UnisonShare.Contribution exposing
    ( Contribution
    , ContributionDetails
    , ContributionStateToken(..)
    , ContributionSummary
    , dateOfHistoricDiffSupport
    , decodeDetails
    , decodeSummary
    , toDetails
    , toSummary
    )

import Code.BranchRef as BranchRef exposing (BranchRef)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Lib.UserHandle as UserHandle
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Check as Check exposing (Check)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.User as User exposing (UserSummary)


{-| Fetched with a contribution and part of the params to merge a contribution.
If the frontend and backend ever differ on what the token is, it means that
there was a change to the contribution in the time between the contribution
was fetched and the user pressed the "merge" button.
-}
type ContributionStateToken
    = ContributionStateToken String


type alias Contribution c =
    { c
        | ref : ContributionRef
        , author : Maybe UserSummary
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


type alias ContributionSummary =
    Contribution {}


type alias ContributionDetails =
    Contribution
        { contributionStateToken : ContributionStateToken
        , latestCheckOnSourceBranch : Maybe Check
        }



-- HELPERS


toSummary : ContributionDetails -> ContributionSummary
toSummary contrib =
    { ref = contrib.ref
    , author = contrib.author
    , sourceBranchRef = contrib.sourceBranchRef
    , targetBranchRef = contrib.targetBranchRef
    , projectRef = contrib.projectRef
    , createdAt = contrib.createdAt
    , updatedAt = contrib.updatedAt
    , status = contrib.status
    , numComments = contrib.numComments
    , title = contrib.title
    , description = contrib.description
    }


toDetails : ContributionStateToken -> Maybe Check -> ContributionSummary -> ContributionDetails
toDetails token check contrib =
    { ref = contrib.ref
    , author = contrib.author
    , sourceBranchRef = contrib.sourceBranchRef
    , targetBranchRef = contrib.targetBranchRef
    , projectRef = contrib.projectRef
    , createdAt = contrib.createdAt
    , updatedAt = contrib.updatedAt
    , status = contrib.status
    , numComments = contrib.numComments
    , title = contrib.title
    , description = contrib.description
    , contributionStateToken = token
    , latestCheckOnSourceBranch = check
    }


dateOfHistoricDiffSupport : DateTime
dateOfHistoricDiffSupport =
    DateTime.unsafeFromISO8601 "2024-04-30T00:00:00.001Z"



-- DECODE


decodeDetails : Decode.Decoder ContributionDetails
decodeDetails =
    let
        emptyUser h =
            { handle = h
            , name = Nothing
            , avatarUrl = Nothing
            , pronouns = Nothing
            }

        decodeAuthor =
            Decode.oneOf
                [ Decode.map emptyUser UserHandle.decodeUnprefixed
                , User.decodeSummary
                ]

        makeContributionDetails ref author sourceBranchRef targetBranchRef projectRef createdAt updatedAt status numComments title description contributionStateToken check =
            { ref = ref
            , author = author
            , sourceBranchRef = sourceBranchRef
            , targetBranchRef = targetBranchRef
            , projectRef = projectRef
            , createdAt = createdAt
            , updatedAt = updatedAt
            , status = status
            , numComments = numComments
            , title = title
            , description = description
            , contributionStateToken = contributionStateToken
            , latestCheckOnSourceBranch = check
            }
    in
    Decode.succeed makeContributionDetails
        |> required "number" ContributionRef.decode
        |> optional "author" (Decode.map Just decodeAuthor) Nothing
        |> required "sourceBranchRef" BranchRef.decode
        |> required "targetBranchRef" BranchRef.decode
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" ContributionStatus.decode
        |> required "numComments" Decode.int
        |> required "title" Decode.string
        |> optional "description" (Decode.map Just Decode.string) Nothing
        |> required "contributionStateToken"
            (Decode.map ContributionStateToken Decode.string)
        |> optional "latestCheckOnSourceBranch" (Decode.map Just Check.decode) Nothing


decodeSummary : Decode.Decoder ContributionSummary
decodeSummary =
    let
        emptyUser h =
            { handle = h
            , name = Nothing
            , avatarUrl = Nothing
            , pronouns = Nothing
            }

        decodeAuthor =
            Decode.oneOf
                [ Decode.map emptyUser UserHandle.decodeUnprefixed
                , User.decodeSummary
                ]

        makeContributionSummary ref author sourceBranchRef targetBranchRef projectRef createdAt updatedAt status numComments title description =
            { ref = ref
            , author = author
            , sourceBranchRef = sourceBranchRef
            , targetBranchRef = targetBranchRef
            , projectRef = projectRef
            , createdAt = createdAt
            , updatedAt = updatedAt
            , status = status
            , numComments = numComments
            , title = title
            , description = description
            }
    in
    Decode.succeed makeContributionSummary
        |> required "number" ContributionRef.decode
        |> optional "author" (Decode.map Just decodeAuthor) Nothing
        |> required "sourceBranchRef" BranchRef.decode
        |> required "targetBranchRef" BranchRef.decode
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" ContributionStatus.decode
        |> required "numComments" Decode.int
        |> required "title" Decode.string
        |> optional "description" (Decode.map Just Decode.string) Nothing
