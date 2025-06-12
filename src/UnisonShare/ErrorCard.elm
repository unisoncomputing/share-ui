module UnisonShare.ErrorCard exposing (..)

import Html exposing (Html, details, div, summary, text)
import Http
import Lib.Util as Util
import UI
import UI.Card as Card exposing (Card)
import UI.StatusBanner as StatusBanner
import UnisonShare.Session as Session exposing (Session)


card : Session -> Http.Error -> String -> String -> Card msg
card session error entityName className =
    let
        errorDetails =
            if Session.isSuperAdmin session then
                details [] [ summary [] [ text "Error Details" ], div [] [ text (Util.httpErrorToString error) ] ]

            else
                UI.nothing

        -- details [] [ summary [] [ text "Error Details" ], div [] [ text (Util.httpErrorToString error) ] ]
    in
    Card.card
        [ StatusBanner.bad ("Something broke on our end and we couldn't show the " ++ entityName ++ ". Please try again.")
        , errorDetails
        ]
        |> Card.withClassName className
        |> Card.asContained


view : Session -> Http.Error -> String -> String -> Html msg
view session error entityName className =
    card session error entityName className
        |> Card.view
