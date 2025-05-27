module UnisonShare.Paginated exposing (..)


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
