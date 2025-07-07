module UnisonShare.AppContext exposing (..)

import Browser.Navigation as Nav
import Code.Config
import Code.Perspective exposing (Perspective)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Time
import UI.DateTime exposing (DateTime)
import UnisonShare.Api as Api
import UnisonShare.CodeBrowsingContext exposing (CodeBrowsingContext)
import UnisonShare.Session exposing (Session)
import Url exposing (Url)


type LastActiveNotificationsTab
    = AllNotifications
    | UnreadNotifications


type alias AppContext =
    { operatingSystem : OperatingSystem
    , basePath : String
    , currentUrl : Url
    , api : HttpApi
    , websiteApi : HttpApi
    , navKey : Nav.Key
    , session : Session
    , whatsNewReadPostIds : List String
    , now : DateTime
    , timeZone : Time.Zone
    , lastActiveNotificationsTab : LastActiveNotificationsTab
    }


type alias Flags =
    { operatingSystem : String
    , basePath : String
    , apiUrl : String
    , websiteUrl : String
    , xsrfToken : Maybe String
    , appEnv : String
    , whatsNewReadPostIds : List String
    , lastActiveNotificationsTab : Maybe String
    }


init : Flags -> Nav.Key -> Url -> DateTime -> Time.Zone -> Session -> AppContext
init flags navKey currentUrl now timeZone session =
    let
        api =
            HttpApi.httpApi True flags.apiUrl flags.xsrfToken
    in
    { operatingSystem = OS.fromString flags.operatingSystem
    , basePath = flags.basePath
    , currentUrl = currentUrl
    , api = api
    , websiteApi = HttpApi.httpApi False flags.websiteUrl Nothing
    , navKey = navKey
    , session = session
    , whatsNewReadPostIds = flags.whatsNewReadPostIds
    , now = now
    , timeZone = timeZone
    , lastActiveNotificationsTab = lastActiveNotificationsTabFromString flags.lastActiveNotificationsTab
    }


lastActiveNotificationsTabFromString : Maybe String -> LastActiveNotificationsTab
lastActiveNotificationsTabFromString raw =
    case raw of
        Just "UnreadNotifications" ->
            UnreadNotifications

        _ ->
            AllNotifications


lastActiveNotificationsTabToString : LastActiveNotificationsTab -> String
lastActiveNotificationsTabToString tab =
    case tab of
        UnreadNotifications ->
            "UnreadNotifications"

        _ ->
            "AllNotifications"


toCodeConfig : AppContext -> CodeBrowsingContext -> Perspective -> Code.Config.Config
toCodeConfig appContex codeBrowsingContext perspective =
    { operatingSystem = appContex.operatingSystem
    , perspective = perspective
    , toApiEndpoint = Api.codebaseApiEndpointToEndpoint codeBrowsingContext
    , api = appContex.api
    }
