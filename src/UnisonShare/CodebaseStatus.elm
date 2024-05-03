module UnisonShare.CodebaseStatus exposing (..)

import Code.Perspective as Perspective
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.CodeBrowsingContext exposing (CodeBrowsingContext)


type CodebaseStatus
    = Empty
    | NotEmpty


fromIsEmpty : Bool -> CodebaseStatus
fromIsEmpty isEmpty_ =
    if isEmpty_ then
        Empty

    else
        NotEmpty


isEmpty : CodebaseStatus -> Bool
isEmpty status =
    status == Empty


checkStatus : (HttpResult CodebaseStatus -> msg) -> AppContext -> CodeBrowsingContext -> Cmd msg
checkStatus msg appContext context =
    let
        decoder =
            -- Parse to "not-empty" since we don't actually care about the values, just that they exist
            Decode.map (List.isEmpty >> fromIsEmpty)
                (Decode.field "namespaceListingChildren"
                    (Decode.list (Decode.succeed "not-empty"))
                )
    in
    ShareApi.browseCodebase context
        Perspective.relativeRootPerspective
        Nothing
        |> HttpApi.toRequest decoder msg
        |> HttpApi.perform appContext.api
