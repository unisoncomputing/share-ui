module UnisonShare.Contribution.ContributionStatus exposing
    ( ContributionStatus(..)
    , decode
    , toApiString
    )

import Json.Decode as Decode
import Json.Decode.Extra exposing (when)


type ContributionStatus
    = Draft
    | InReview
    | Merged
    | Archived


toApiString : ContributionStatus -> String
toApiString status =
    case status of
        Draft ->
            "draft"

        InReview ->
            "in_review"

        Merged ->
            "merged"

        Archived ->
            "closed"



-- DECODE


decode : Decode.Decoder ContributionStatus
decode =
    Decode.oneOf
        [ when Decode.string ((==) "draft") (Decode.succeed Draft)
        , when Decode.string ((==) "in_review") (Decode.succeed InReview)
        , when Decode.string ((==) "merged") (Decode.succeed Merged)
        , when Decode.string ((==) "closed") (Decode.succeed Archived)
        ]
