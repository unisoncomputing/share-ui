module UnisonShare.Page.PrivacyPolicyPage exposing (..)

import Html exposing (div)
import Html.Attributes exposing (class)
import Markdown
import UI.Card as Card
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
            Card.card [ div [ class "definition-doc" ] [ Markdown.toHtml [] "require:src/privacy-policy.md" ] ]
                |> Card.asContained
                |> Card.view

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn [ content ]
                    |> PageContent.withPageTitle
                        (PageTitle.title "Unison Computing Privacy Policy")
                )
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground
    in
    { pageId = "privacy-policy"
    , title = "Privacy Policy"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
