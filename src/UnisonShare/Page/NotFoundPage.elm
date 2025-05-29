module UnisonShare.Page.NotFoundPage exposing (..)

import Html exposing (text)
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.PageFooter as PageFooter


view : AppDocument msg
view =
    let
        content =
            [ Card.card [ text "Sorry, we can't find that page." ] |> Card.view
            ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle
                        (PageTitle.title "Page not found" |> PageTitle.withIcon Icon.warn)
                )
                PageFooter.pageFooter
    in
    { pageId = "not-found"
    , title = "Page not found"
    , appHeader = AppHeader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
