module UnisonShare.ProjectWebhook exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Lib.Decode.Helpers as DecodeH exposing (whenFieldIs)
import UnisonShare.NotificationTopicType as NotificationTopicType exposing (NotificationTopicType)
import Url exposing (Url)


type ProjectWebhookTopics
    = AllTopics
    | SelectedTopics (List NotificationTopicType)


type alias ProjectWebhookForm =
    { url : Url
    , topics : ProjectWebhookTopics
    }


type alias ProjectWebhook =
    { id : String
    , url : Url
    , topics : ProjectWebhookTopics
    }



-- ENCODE


encodeTopics : ProjectWebhookTopics -> Encode.Value
encodeTopics topic =
    case topic of
        AllTopics ->
            Encode.object [ ( "type", Encode.string "all" ) ]

        SelectedTopics selectedTopics ->
            Encode.object
                [ ( "type", Encode.string "selected" )
                , ( "topics"
                  , Encode.list
                        (NotificationTopicType.toApiString >> Encode.string)
                        selectedTopics
                  )
                ]


decodeTopics : Decode.Decoder ProjectWebhookTopics
decodeTopics =
    Decode.oneOf
        [ whenFieldIs "type" "all" (Decode.succeed AllTopics)
        , whenFieldIs "type" "selected" (Decode.map SelectedTopics (Decode.field "topics" (Decode.list NotificationTopicType.decode)))
        ]


decode : Decode.Decoder ProjectWebhook
decode =
    Decode.succeed ProjectWebhook
        |> required "notificationSubscriptionId" Decode.string
        |> required "uri" DecodeH.url
        |> required "topics" decodeTopics
