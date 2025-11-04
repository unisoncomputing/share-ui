module UnisonShare.Project.BranchHistory exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (maybeAt, whenTagIs)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.User as User exposing (UserSummary)


type CommentAuthor
    = VerifiedShareUser UserSummary
    | UnverifiedUser { name : String }


type HistoryEntry
    = Comment CommentDetails
    | Changeset ChangesetDetails


type alias Change =
    { hash : Hash
    , fqn : FQN
    }


type alias CommentDetails =
    { createdAt : DateTime
    , afterCausalHash : Hash
    , author : CommentAuthor
    , subject : Maybe String
    , body : String
    }


type alias ChangesetDetails =
    { hash : Hash
    , updates : List Change
    , removes : List Change
    , aliases : List Change
    , renames : List Change
    }


type alias BranchHistory =
    List HistoryEntry



-- DECODE


decodeComment : Decode.Decoder HistoryEntry
decodeComment =
    let
        decodeAuthor =
            Decode.oneOf
                [ whenTagIs "ShareUser" (Decode.map VerifiedShareUser User.decodeSummary)
                , whenTagIs "UnverifiedUser"
                    (Decode.map
                        (\n -> UnverifiedUser { name = n })
                        (Decode.field "name" Decode.string)
                    )
                ]

        toComment createdAt afterCausalHash author subject body =
            Comment
                { createdAt = createdAt
                , afterCausalHash = afterCausalHash
                , author = author
                , subject = subject
                , body = body
                }
    in
    Decode.succeed toComment
        |> required "createdAt" DateTime.decode
        |> required "afterCausalHash" Hash.decode
        |> required "author" decodeAuthor
        |> maybeAt [ "comment", "subject" ] Decode.string
        |> requiredAt [ "comment", "body" ] Decode.string


decodeChangeset : Decode.Decoder HistoryEntry
decodeChangeset =
    let
        decodeChange =
            Decode.succeed Change
                |> required "hash" Hash.decode
                |> required "fqn" FQN.decode

        toChangeset hash updates removes aliases renames =
            Changeset
                { hash = hash
                , updates = updates
                , removes = removes
                , aliases = aliases
                , renames = renames
                }
    in
    Decode.succeed toChangeset
        |> required "hash" Hash.decode
        |> required "updates" (Decode.list decodeChange)
        |> required "removes" (Decode.list decodeChange)
        |> required "aliases" (Decode.list decodeChange)
        |> required "renames" (Decode.list decodeChange)


decodeHistoryEntry : Decode.Decoder HistoryEntry
decodeHistoryEntry =
    Decode.oneOf
        [ whenTagIs "Comment" decodeComment
        , whenTagIs "Changeset" decodeChangeset
        ]


decode : Decode.Decoder BranchHistory
decode =
    Decode.field "history" (Decode.list decodeHistoryEntry)
