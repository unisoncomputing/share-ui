module UnisonShare.AppDocument exposing (AppDocument, appDocument, map, view, withModal)

import Browser exposing (Document)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Maybe.Extra as MaybeE
import UI
import UI.Button as Button
import UI.PageHeader as PageHeader exposing (PageHeader)
import UnisonShare.AppHeader as AppHeader exposing (AppHeader, AppHeaderContext)
import UnisonShare.Link as Link



{-

   AppDocument
   ===========

   Very similar to Browser.Document, but includes a common app title and app
   frame, as well as slots for header, page, and modals.
-}


type alias AppDocument msg =
    { pageId : String
    , title : String
    , appHeader : AppHeader
    , pageHeader : Maybe (PageHeader msg)
    , page : Html msg
    , modal : Maybe (Html msg)
    }



-- CREATE


appDocument : String -> String -> AppHeader -> Html msg -> AppDocument msg
appDocument pageId title appHeader page =
    { pageId = pageId
    , title = title
    , appHeader = appHeader
    , pageHeader = Nothing
    , page = page
    , modal = Nothing
    }



-- MODIFY


withModal : Html msg -> AppDocument msg -> AppDocument msg
withModal modal appDoc =
    { appDoc | modal = Just modal }



-- MAP


map : (msgA -> msgB) -> AppDocument msgA -> AppDocument msgB
map toMsgB { pageId, title, appHeader, pageHeader, page, modal } =
    { pageId = pageId
    , title = title
    , appHeader = appHeader
    , pageHeader = Maybe.map (PageHeader.map toMsgB) pageHeader
    , page = Html.map toMsgB page
    , modal = Maybe.map (Html.map toMsgB) modal
    }



-- VIEW
{- viewAnnouncement

   Example when enabled:

       Just (
         div [ id "announcement" ]
             [ div [ class "announcement_content" ]
                 [ text "🎉☁️ "
                 , Link.view "Unison Cloud" Link.unisonCloudWebsite
                 , text " is generally available!"
                 ]
             ]
         )
-}


viewAnnouncement : Maybe (Html msg)
viewAnnouncement =
    Just
        (div [ id "announcement" ]
            [ div [ class "announcement_content" ]
                [ Link.view
                    "🎄 Season's Greetings! Join us for this year's Advent of Code puzzles."
                    Link.aoc2024
                , Button.button_ Link.aoc2024 "Get started"
                    |> Button.emphasized
                    |> Button.small
                    |> Button.view
                ]
            ]
        )


view : AppHeaderContext msg -> AppDocument msg -> Document msg
view appHeaderCtx { pageId, title, appHeader, pageHeader, page, modal } =
    { title = title ++ " | Unison Share"
    , body =
        [ div
            [ id "app", class pageId ]
            [ Maybe.withDefault UI.nothing viewAnnouncement
            , AppHeader.view appHeaderCtx appHeader
            , MaybeE.unwrap UI.nothing PageHeader.view pageHeader
            , page
            , Maybe.withDefault UI.nothing modal
            ]
        ]
    }
