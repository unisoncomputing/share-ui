module UnisonShare.Contribution.ContributionMergeability exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra exposing (when)


type ContributionMergeability
    = FastForward
    | Mergeable
    | Conflicted
    | AlreadyMerged
    | NotMergeable


isMergeable : ContributionMergeability -> Bool
isMergeable mergeability =
    mergeability == FastForward || mergeability == Mergeable


decode : Decode.Decoder ContributionMergeability
decode =
    Decode.oneOf
        [ when Decode.string ((==) "fast_forward") (Decode.succeed FastForward)
        , when Decode.string ((==) "merge") (Decode.succeed Mergeable)
        , when Decode.string ((==) "conflicted") (Decode.succeed Conflicted)
        , when Decode.string ((==) "already_merged") (Decode.succeed AlreadyMerged)
        , when Decode.string ((==) "cant_merge") (Decode.succeed NotMergeable)
        ]
