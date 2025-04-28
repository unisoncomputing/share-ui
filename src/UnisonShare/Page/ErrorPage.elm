module UnisonShare.Page.ErrorPage exposing (..)

import Html exposing (details, div, summary, text)
import Http
import Lib.Util as Util
import UI
import UI.Card as Card
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout exposing (PageLayout)
import UI.PageTitle as PageTitle
import UI.StatusBanner as StatusBanner
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Session as Session exposing (Session)


view : Session -> Http.Error -> String -> String -> PageLayout msg
view session error entityName className =
    let
        errorDetails =
            if Session.isSuperAdmin session then
                details [] [ summary [] [ text "Error Details" ], div [] [ text (Util.httpErrorToString error) ] ]

            else
                UI.nothing
    in
    PageLayout.centeredNarrowLayout
        (PageContent.oneColumn
            [ Card.card
                [ StatusBanner.bad ("Something broke on our end and we couldn't show the " ++ entityName ++ ". Please try again.")
                , errorDetails
                ]
                |> Card.withClassName className
                |> Card.asContained
                |> Card.view
            ]
            |> PageContent.withPageTitle (PageTitle.title "Error")
        )
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground
