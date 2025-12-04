module UnisonShare.Check exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Hash as Hash exposing (Hash)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Lib.Decode.Helpers exposing (url, whenTagIs)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Parser exposing (Parser)
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import Url exposing (Url)


type CheckStatus
    = NotStarted
    | Waiting { startedAt : DateTime }
    | TimeOut
        { startedAt : DateTime
        , endedAt : DateTime
        }
    | Failure
        { startedAt : DateTime
        , endedAt : DateTime
        , errorTitle : String
        , errorDetails : String
        , output : String
        }
    | Success
        { startedAt : DateTime
        , endedAt : DateTime
        , output : String
        }


type Runner
    = LocalUcm UserHandle
    | Webhook Url


type alias Check =
    { id : CheckId
    , runner : Runner
    , projectRef : ProjectRef
    , branchRef : BranchRef
    , causalHash : Hash
    , createdAt : DateTime
    , updatedAt : DateTime
    , status : CheckStatus
    }



-- CheckId


type CheckId
    = CheckId String


checkIdToString : CheckId -> String
checkIdToString (CheckId raw) =
    raw


{-| TODO
-}
checkIdFromString : String -> Maybe CheckId
checkIdFromString raw =
    Just (CheckId raw)


checkIdFromUrl : Parser CheckId
checkIdFromUrl =
    let
        parseMaybe checkId =
            case checkId of
                Just checkId_ ->
                    Parser.succeed checkId_

                Nothing ->
                    Parser.problem "Invalid CheckId"
    in
    Parser.chompUntilEndOr "/"
        |> Parser.getChompedString
        |> Parser.map checkIdFromString
        |> Parser.andThen parseMaybe


title : Check -> String
title check =
    ""



-- DECODE


decodeRunner : Decode.Decoder Runner
decodeRunner =
    Decode.oneOf
        [ whenTagIs "Local" (Decode.map LocalUcm (Decode.field "handle" UserHandle.decode))
        , whenTagIs "Webhook" (Decode.map Webhook (Decode.field "url" url))
        ]


decodeStatus : Decode.Decoder CheckStatus
decodeStatus =
    let
        decodeWaiting =
            Decode.succeed (\s -> Waiting { startedAt = s })
                |> required "startedAt" DateTime.decode

        decodeTimeOut =
            Decode.succeed (\s e -> TimeOut { startedAt = s, endedAt = e })
                |> required "startedAt" DateTime.decode
                |> required "endedAt" DateTime.decode

        decodeFailure =
            let
                mkFailure started ended error errorDetails output =
                    Failure
                        { startedAt = started
                        , endedAt = ended
                        , errorTitle = error
                        , errorDetails = errorDetails
                        , output = output
                        }
            in
            Decode.succeed mkFailure
                |> required "startedAt" DateTime.decode
                |> required "endedAt" DateTime.decode
                |> required "errorTitle" Decode.string
                |> required "errorDetails" Decode.string
                |> required "output" Decode.string

        decodeSuccess =
            Decode.succeed (\s e o -> Success { startedAt = s, endedAt = e, output = o })
                |> required "startedAt" DateTime.decode
                |> required "endedAt" DateTime.decode
                |> required "output" Decode.string
    in
    Decode.oneOf
        [ whenTagIs "NotStarted" (Decode.succeed NotStarted)
        , whenTagIs "Waiting" decodeWaiting
        , whenTagIs "TimeOut" decodeTimeOut
        , whenTagIs "Failure" decodeFailure
        , whenTagIs "Success" decodeSuccess
        ]


decodeCheckId : Decode.Decoder CheckId
decodeCheckId =
    Decode.map CheckId Decode.string


decode : Decode.Decoder Check
decode =
    let
        make id runner projectRef branchRef causalHash createdAt updatedAt status =
            { id = id
            , runner = runner
            , projectRef = projectRef
            , branchRef = branchRef
            , causalHash = causalHash
            , createdAt = createdAt
            , updatedAt = updatedAt
            , status = status
            }
    in
    Decode.succeed make
        |> required "id" decodeCheckId
        |> required "runner" decodeRunner
        |> required "projectRef" ProjectRef.decode
        |> required "branchRef" BranchRef.decode
        |> required "causalHash" Hash.decode
        |> required "createdAt" DateTime.decode
        |> required "updatedAt" DateTime.decode
        |> required "status" decodeStatus
