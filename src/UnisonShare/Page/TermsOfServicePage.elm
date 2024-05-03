module UnisonShare.Page.TermsOfServicePage exposing (..)

import Html exposing (div)
import Html.Attributes exposing (class)
import Markdown
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
            Card.card [ div [ class "definition-doc" ] [ Markdown.toHtml [] "require:src/terms-of-service.md" ] ]
                |> Card.asContained
                |> Card.view

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ content ]
                    |> PageContent.withPageTitle
                        (PageTitle.title "Terms of Service"
                            |> PageTitle.withIcon Icon.documentCertificate
                        )
                )
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
    in
    { pageId = "terms-of-service"
    , title = "Terms of Service"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
