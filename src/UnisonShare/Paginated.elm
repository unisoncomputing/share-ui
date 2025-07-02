module UnisonShare.Paginated exposing (..)

import Html exposing (Html, footer)
import Html.Attributes exposing (class)
import UI.Button as Button
import UI.Click as Click exposing (Click)
import UI.Icon as Icon
import Url.Builder exposing (QueryParameter, string)


type PageCursorParam
    = NoPageCursor
    | PrevPage PageCursor
    | NextPage PageCursor


type PageCursor
    = PageCursor String


type Paginated a
    = Paginated
        { prev : Maybe PageCursor
        , next : Maybe PageCursor
        , items : List a
        }


cursorToString : PageCursor -> String
cursorToString (PageCursor c) =
    c


toQueryParam : PageCursorParam -> Maybe QueryParameter
toQueryParam param =
    case param of
        NoPageCursor ->
            Nothing

        PrevPage c ->
            Just (string "prev" (cursorToString c))

        NextPage c ->
            Just (string "next" (cursorToString c))


toQueryParams : PageCursorParam -> List QueryParameter
toQueryParams param =
    param
        |> toQueryParam
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


view : (PageCursorParam -> Click msg) -> { prev : Maybe PageCursor, next : Maybe PageCursor } -> Html msg
view toClick cursors =
    let
        paginationButton icon click =
            Button.icon_ click icon

        buttons =
            case ( cursors.prev, cursors.next ) of
                ( Just prev, Just next ) ->
                    [ paginationButton Icon.arrowLeft (toClick (PrevPage prev))
                    , paginationButton Icon.arrowRight (toClick (NextPage next))
                    ]

                ( Just prev, Nothing ) ->
                    [ paginationButton Icon.arrowLeft (toClick (PrevPage prev))
                    , paginationButton Icon.arrowRight Click.disabled
                    ]

                ( Nothing, Just next ) ->
                    [ paginationButton Icon.arrowLeft Click.disabled
                    , paginationButton Icon.arrowRight (toClick (NextPage next))
                    ]

                ( Nothing, Nothing ) ->
                    [ paginationButton Icon.arrowLeft Click.disabled
                    , paginationButton Icon.arrowRight Click.disabled
                    ]
    in
    footer [ class "paginated" ] (List.map (Button.small >> Button.view) buttons)
