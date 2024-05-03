module WebsiteApi exposing (..)

import Lib.HttpApi exposing (Endpoint(..))


whatsNewFeed : Endpoint
whatsNewFeed =
    GET { path = [ "feed.json" ], queryParams = [] }
