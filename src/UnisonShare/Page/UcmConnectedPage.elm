module UnisonShare.Page.UcmConnectedPage exposing (..)

import Html exposing (br, footer, h1, p, text)
import Html.Attributes exposing (class)
import UI.Button as Button
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.StatusIndicator as StatusIndicator
import UnisonShare.Account exposing (AccountSummary)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter


view : AccountSummary -> AppDocument msg
view account =
    let
        content =
            [ StatusIndicator.good |> StatusIndicator.large |> StatusIndicator.view
            , h1 [] [ text "Sweet! You’ve successfully connected UCM and Unison Share 🎉" ]
            , p
                []
                [ text "Close this window to get back to your UCM session,"
                , br [] []
                , text "or feel free to explore Unison Share."
                ]
            , footer [ class "actions" ]
                [ Button.iconThenLabel_ Link.catalog Icon.window "Project Catalog" |> Button.medium |> Button.view
                , Button.iconThenLabel_ (Link.userCodeRoot account.handle) Icon.chest "Your Codebase" |> Button.medium |> Button.view
                , Button.iconThenLabel_ Link.unisonShareDocs Icon.graduationCap "Code Hosting Guide" |> Button.medium |> Button.view
                ]
            ]

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn content)
                PageFooter.pageFooter
    in
    { pageId = "ucm-connected"
    , title = "UCM Connected"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
