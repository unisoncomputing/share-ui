module UnisonShare.Project.Release exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Doc exposing (Doc)
import Code.Hash as Hash exposing (Hash)
import Code.Version as Version exposing (Version)
import Json.Decode as Decode exposing (field, string)
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.User as User exposing (UserSummary)


type alias StatusMeta =
    { at : DateTime, by : UserSummary }


type ReleaseStatus
    = Draft
    | Published StatusMeta
    | Unpublished StatusMeta


type alias Release =
    { version : Version
    , causalHashUnsquashed : Hash
    , causalHashSquashed : Hash
    , releaseNotes : Maybe Doc
    , projectRef : ProjectRef
    , createdAt : DateTime
    , createdBy : UserHandle
    , updatedAt : DateTime
    , status : ReleaseStatus
    }



-- HELPERS


isDraft : Release -> Bool
isDraft r =
    case r.status of
        Draft ->
            True

        _ ->
            False


isPublished : Release -> Bool
isPublished r =
    case r.status of
        Published _ ->
            True

        _ ->
            False


isUnpublished : Release -> Bool
isUnpublished r =
    case r.status of
        Unpublished _ ->
            True

        _ ->
            False


branchRef : Release -> BranchRef
branchRef r =
    BranchRef.releaseBranchRef r.version



-- DECODE


decode : Decode.Decoder Release
decode =
    let
        published at by =
            Published { at = at, by = by }

        unpublished at by =
            Unpublished { at = at, by = by }

        toEmptyUser handle =
            { handle = handle, name = Nothing, avatarUrl = Nothing, pronouns = Nothing }

        decodeBy =
            Decode.oneOf
                [ Decode.map toEmptyUser UserHandle.decode
                , User.decodeSummary
                ]

        decodeStatus =
            Decode.oneOf
                [ when (field "status" string)
                    ((==) "draft")
                    (Decode.succeed Draft)
                , when (field "status" string)
                    ((==) "published")
                    (Decode.map2 published
                        (field "publishedAt" DateTime.decode)
                        (field "publishedBy" decodeBy)
                    )
                , when (field "status" string)
                    ((==) "deprecated")
                    (Decode.map2 unpublished
                        (field "deprecatedAt" DateTime.decode)
                        (field "deprecatedBy" decodeBy)
                    )
                ]

        release version unsquashed squashed projectRef createdAt createdBy updatedAt status =
            { version = version
            , causalHashUnsquashed = unsquashed
            , causalHashSquashed = squashed
            , releaseNotes = Nothing
            , projectRef = projectRef
            , createdAt = createdAt
            , createdBy = createdBy
            , updatedAt = updatedAt
            , status = status
            }
    in
    Decode.succeed release
        |> required "version" Version.decode
        |> required "causalHashUnsquashed" Hash.decode
        |> required "causalHashSquashed" Hash.decode
        -- |> required "releaseNotes" (nullable Doc.decode)
        |> required "projectRef" ProjectRef.decode
        |> required "createdAt" DateTime.decode
        |> required "createdBy" UserHandle.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" decodeStatus
