module UnisonShare.Timeline.TimelineEvent exposing (..)

import Html exposing (Html, div, header, strong, text)
import Html.Attributes exposing (class)
import UI.DateTime exposing (DateTime)
import UI.Icon as Icon
import UnisonShare.User exposing (UserSummary)


type alias TimelineEventDetails d =
    { d | timestamp : DateTime, actor : UserSummary }



-- VIEW


viewHeader : List (Html msg) -> Html msg
viewHeader headerDescription =
    header [ class "timeline-event_header" ]
        [ div [ class "timeline-event_header_description" ] headerDescription ]


viewIcon : Icon.Icon msg -> Html msg
viewIcon ico =
    div [ class "timeline-event_icon" ] [ Icon.view ico ]


viewTitle : String -> Html msg
viewTitle title =
    strong [ class "timeline-event_title" ] [ text title ]


viewDescription : List (Html msg) -> Html msg
viewDescription description =
    div [ class "timeline-event_description" ] description


view : List (Html msg) -> Html msg
view content =
    div [ class "timeline-event" ] content
