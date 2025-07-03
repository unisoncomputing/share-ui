module UnisonShare.Contribution.ContributionStatus exposing
    ( ContributionStatus(..)
    , decode
    , toApiString
    , view
    )

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import UI.Icon as Icon


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


toString : ContributionStatus -> String
toString status =
    case status of
        Draft ->
            "Draft"

        InReview ->
            "In Review"

        Merged ->
            "Merged"

        Archived ->
            "Archived"


view : ContributionStatus -> Html msg
view status =
    let
        ( className, icon ) =
            case status of
                Draft ->
                    ( "draft", Icon.writingPad )

                InReview ->
                    ( "in_review", Icon.conversation )

                Merged ->
                    ( "merged", Icon.checkmark )

                Archived ->
                    ( "closed", Icon.archive )
    in
    div [ class ("contribution-status " ++ className) ]
        [ Icon.view icon, text (toString status) ]



-- DECODE


decode : Decode.Decoder ContributionStatus
decode =
    Decode.oneOf
        [ when Decode.string ((==) "draft") (Decode.succeed Draft)
        , when Decode.string ((==) "in_review") (Decode.succeed InReview)
        , when Decode.string ((==) "merged") (Decode.succeed Merged)
        , when Decode.string ((==) "closed") (Decode.succeed Archived)
        ]
