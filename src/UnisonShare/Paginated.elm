module UnisonShare.Paginated exposing (..)

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
