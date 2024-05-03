module UnisonShare.Project.ReleaseDownloadsTest exposing (..)

import Expect
import Test exposing (..)
import UnisonShare.Project.ReleaseDownloads as ReleaseDownloads exposing (ReleaseDownloads(..))


weeklyAverage : Test
weeklyAverage =
    describe "ReleaseDownloads.weeklyAverage"
        [ test "calculates the average of the last 4 weeks (avg of avg)" <|
            \_ ->
                Expect.equal
                    9
                    (ReleaseDownloads.weeklyAverage releaseDownloads_)
        ]


releaseDownloads_ : ReleaseDownloads
releaseDownloads_ =
    ReleaseDownloads
        [ 3
        , 4
        , 6
        , 8
        , 22
        , 11
        , 3

        --
        , 6
        , 4
        , 8
        , 13
        , 16
        , 10
        , 9

        --
        , 7
        , 7
        , 9
        , 14
        , 24
        , 10
        , 7

        --
        , 14
        , 8
        , 9
        , 4
        , 2
        , 18
        , 4
        ]
