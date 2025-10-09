module UnisonShare.ErrorCard exposing (..)

import Html exposing (Html)
import Http
import UI.Card as Card exposing (Card)
import UI.StatusBanner as StatusBanner
import UnisonShare.ErrorDetails as ErrorDetails
import UnisonShare.Session exposing (Session)


card : Session -> Http.Error -> String -> String -> Card msg
card session error entityName className =
    Card.card
        [ StatusBanner.bad ("Something broke on our end and we couldn't show the " ++ entityName ++ ". Please try again.")
        , ErrorDetails.view session error
        ]
        |> Card.withClassName className
        |> Card.asContained


view : Session -> Http.Error -> String -> String -> Html msg
view session error entityName className =
    card session error entityName className
        |> Card.view
