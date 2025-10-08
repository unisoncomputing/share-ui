module UnisonShare.NotificationTopicType exposing (..)

import Json.Decode as Decode
import Json.Decode.Extra exposing (when)


type NotificationTopicType
    = ProjectContributionCreated
    | ProjectContributionUpdated
    | ProjectContributionComment
    | ProjectTicketCreated
    | ProjectTicketUpdated
    | ProjectTicketComment
    | ProjectBranchUpdated
    | ProjectReleaseCreated


toApiString : NotificationTopicType -> String
toApiString topic =
    case topic of
        ProjectContributionCreated ->
            "project:contribution:created"

        ProjectContributionUpdated ->
            "project:contribution:updated"

        ProjectContributionComment ->
            "project:contribution:comment"

        ProjectTicketCreated ->
            "project:ticket:created"

        ProjectTicketUpdated ->
            "project:ticket:updated"

        ProjectTicketComment ->
            "project:ticket:comment"

        ProjectBranchUpdated ->
            "project:branch:updated"

        ProjectReleaseCreated ->
            "project:release:created"


toString : NotificationTopicType -> String
toString topic =
    case topic of
        ProjectContributionCreated ->
            "ProjectContributionCreated"

        ProjectContributionUpdated ->
            "ProjectContributionUpdated"

        ProjectContributionComment ->
            "ProjectContributionComment"

        ProjectTicketCreated ->
            "ProjectTicketCreated"

        ProjectTicketUpdated ->
            "ProjectTicketUpdated"

        ProjectTicketComment ->
            "ProjectTicketComment"

        ProjectBranchUpdated ->
            "ProjectBranchUpdated"

        ProjectReleaseCreated ->
            "ProjectReleaseCreated"



-- DECODE


decode : Decode.Decoder NotificationTopicType
decode =
    Decode.oneOf
        [ when Decode.string ((==) "project:ticket:comment") (Decode.succeed ProjectTicketComment)
        , when Decode.string ((==) "project:ticket:created") (Decode.succeed ProjectTicketCreated)
        , when Decode.string ((==) "project:ticket:updated") (Decode.succeed ProjectTicketUpdated)
        , when Decode.string ((==) "project:contribution:comment") (Decode.succeed ProjectContributionComment)
        , when Decode.string ((==) "project:contribution:created") (Decode.succeed ProjectContributionCreated)
        , when Decode.string ((==) "project:contribution:updated") (Decode.succeed ProjectContributionUpdated)
        , when Decode.string ((==) "project:branch:updated") (Decode.succeed ProjectBranchUpdated)
        , when Decode.string ((==) "project:release:created") (Decode.succeed ProjectReleaseCreated)
        ]
