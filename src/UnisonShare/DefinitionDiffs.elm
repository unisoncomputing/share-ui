{- A DefinitionDiff collection for easy access and memoization -}


module UnisonShare.DefinitionDiffs exposing (..)

import Dict exposing (Dict)
import RemoteData exposing (WebData)
import UnisonShare.DefinitionDiff exposing (DefinitionDiff)
import UnisonShare.DefinitionDiffKey as DefinitionDiffKey exposing (DefinitionDiffKey)


type DefinitionDiffs
    = DefinitionDiffs (Dict String (WebData DefinitionDiff))



-- CREATE


empty : DefinitionDiffs
empty =
    DefinitionDiffs Dict.empty


set : DefinitionDiffs -> DefinitionDiffKey -> WebData DefinitionDiff -> DefinitionDiffs
set (DefinitionDiffs diffs) key diff =
    DefinitionDiffs
        (Dict.insert (DefinitionDiffKey.toString key) diff diffs)


get : DefinitionDiffs -> DefinitionDiffKey -> Maybe (WebData DefinitionDiff)
get (DefinitionDiffs diffs) key =
    Dict.get (DefinitionDiffKey.toString key) diffs


remove : DefinitionDiffs -> DefinitionDiffKey -> DefinitionDiffs
remove (DefinitionDiffs diffs) key =
    DefinitionDiffs
        (Dict.remove (DefinitionDiffKey.toString key) diffs)
