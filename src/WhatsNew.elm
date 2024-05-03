port module WhatsNew exposing (..)

import Json.Decode as Decode exposing (field, string)
import Json.Encode as Encode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import List.Extra as ListE
import UI.DateTime as DateTime exposing (DateTime)
import UnisonShare.AppContext exposing (AppContext)
import WebsiteApi


type PostId
    = PostId String


type alias Post =
    { id : PostId
    , title : String
    , summary : String
    , url : String
    , publishedAt : DateTime
    }


type alias ReadPostIds =
    List PostId


type alias LoadedWhatsNew =
    { posts : List Post
    , readPostIds : ReadPostIds
    }


type WhatsNew
    = Loading ReadPostIds
    | Success LoadedWhatsNew
    | Failure ReadPostIds


isUnread : LoadedWhatsNew -> Post -> Bool
isUnread whatsNew post =
    isUnread_ whatsNew.readPostIds post.id


isUnread_ : ReadPostIds -> PostId -> Bool
isUnread_ readPostIds postId =
    not (List.member postId readPostIds)


hasAnyUnreadPosts : WhatsNew -> Bool
hasAnyUnreadPosts whatsNew =
    case whatsNew of
        Success wn ->
            List.any (.id >> isUnread_ wn.readPostIds) wn.posts

        _ ->
            False



-- DECODE


decodePost : Decode.Decoder Post
decodePost =
    Decode.map5
        Post
        (Decode.map PostId (field "id" string))
        (field "title" string)
        (field "summary" string)
        (field "url" string)
        (field "date_published" DateTime.decode)


decode : WhatsNew -> Decode.Decoder LoadedWhatsNew
decode whatsNew =
    let
        readPostIds =
            case whatsNew of
                Loading readPostIds_ ->
                    readPostIds_

                Success d ->
                    d.readPostIds

                Failure readPostIds_ ->
                    readPostIds_
    in
    Decode.map (\p -> LoadedWhatsNew p readPostIds) (field "items" (Decode.list decodePost))


postIdToString : PostId -> String
postIdToString (PostId postId) =
    postId



-- EFFECTS


fetchFeed : AppContext -> WhatsNew -> (HttpResult LoadedWhatsNew -> msg) -> Cmd msg
fetchFeed appContext whatsNew msg =
    WebsiteApi.whatsNewFeed
        |> HttpApi.toRequest (decode whatsNew) msg
        |> HttpApi.perform appContext.websiteApi


markAllAsRead : WhatsNew -> ( WhatsNew, Cmd msg )
markAllAsRead whatsNew =
    case whatsNew of
        Success wn ->
            let
                allReadPostIds =
                    (List.map .id wn.posts
                        ++ wn.readPostIds
                    )
                        |> ListE.uniqueBy postIdToString
            in
            ( Success { wn | readPostIds = allReadPostIds }
            , allReadPostIds
                |> encode
                |> updateWhatsNewReadPostIds
            )

        _ ->
            ( whatsNew, Cmd.none )



-- PORTS


encode : List PostId -> Encode.Value
encode postIds =
    let
        encodePostId (PostId p) =
            Encode.string p
    in
    Encode.list encodePostId postIds


port updateWhatsNewReadPostIds : Encode.Value -> Cmd msg
