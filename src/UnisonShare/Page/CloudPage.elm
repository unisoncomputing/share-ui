module UnisonShare.Page.CloudPage exposing (..)

import Html exposing (h1, p, text)
import UI.Card as Card
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.SupportChatWidget as SupportChatWidget


view : Session -> AppDocument msg
view session =
    let
        content =
            case session of
                Session.Anonymous ->
                    PageContent.oneColumn
                        [ text "Visit "
                        , Link.view "unison.cloud" Link.unisonCloudWebsite
                        , text " for more details."
                        ]

                Session.SignedIn a ->
                    PageContent.oneColumn
                        [ Card.card
                            [ h1 [] [ text "Unison Cloud" ]
                            , p [] [ text "Unison Cloud is currently in early beta." ]
                            , p [] [ text "Reach out to us with the chat widget below." ]
                            ]
                            |> Card.view
                        , Link.view "Learn more about Unison Cloud." Link.unisonCloudWebsite
                        , SupportChatWidget.view a
                        ]

        page =
            PageLayout.centeredLayout content PageFooter.pageFooter
    in
    { pageId = "cloud-page"
    , title = "Unison Cloud"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
