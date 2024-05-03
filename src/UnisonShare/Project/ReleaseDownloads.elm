module UnisonShare.Project.ReleaseDownloads exposing
    ( ReleaseDownloads(..)
    , decode
    , fourWeekTotal
    , weeklyAverage
    )

import Json.Decode as Decode


type ReleaseDownloads
    = ReleaseDownloads (List Int)


fourWeekTotal : ReleaseDownloads -> Int
fourWeekTotal (ReleaseDownloads downloads) =
    List.sum downloads


weeklyAverage : ReleaseDownloads -> Int
weeklyAverage (ReleaseDownloads downloads) =
    let
        avg : List Int -> Int
        avg l =
            List.sum l // List.length l
    in
    avg downloads



-- DECODE


decode : Decode.Decoder ReleaseDownloads
decode =
    Decode.map ReleaseDownloads (Decode.list Decode.int)
