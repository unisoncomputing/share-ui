module UnisonShare.Timeline.CommentId exposing (CommentId, decode, equals, fromString, toString)

import Json.Decode as Decode


type CommentId
    = CommentId String


fromString : String -> CommentId
fromString =
    CommentId


toString : CommentId -> String
toString (CommentId commentId) =
    commentId


equals : CommentId -> CommentId -> Bool
equals (CommentId a) (CommentId b) =
    a == b


decode : Decode.Decoder CommentId
decode =
    Decode.map CommentId Decode.string
