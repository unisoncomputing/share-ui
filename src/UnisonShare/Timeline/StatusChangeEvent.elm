module UnisonShare.Timeline.StatusChangeEvent exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (class)
import UI.ByAt as ByAt
import UI.Icon exposing (Icon)
import UnisonShare.DateTimeContext exposing (DateTimeContext)
import UnisonShare.Timeline.TimelineEvent as TimelineEvent exposing (TimelineEventDetails)


view : DateTimeContext a -> Icon msg -> String -> TimelineEventDetails d -> Html msg
view dtContext icon title { actor, timestamp } =
    let
        byAt =
            ByAt.byAt actor timestamp
                |> ByAt.view dtContext.timeZone dtContext.now
    in
    div [ class "timeline-event_status-change" ]
        [ TimelineEvent.viewHeader
            [ TimelineEvent.viewIcon icon
            , TimelineEvent.viewDescription [ TimelineEvent.viewTitle title ]
            , byAt
            ]
        ]
