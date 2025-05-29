module UnisonShare.PreApp exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, id, title)
import Http
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.Util as Util
import Task exposing (Task)
import Time
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UnisonShare.Api as ShareApi
import UnisonShare.App as App
import UnisonShare.AppContext as AppContext exposing (Flags)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Route as Route exposing (Route)
import UnisonShare.Session as Session exposing (Session)
import Url exposing (Url)


type Model
    = Initializing PreAppContext
    | InitializationError PreAppContext Http.Error
    | Initialized App.Model


type alias PreAppContext =
    { flags : Flags
    , route : Route
    , currentUrl : Url
    , navKey : Nav.Key
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        route =
            Route.fromUrl flags.basePath url

        preAppContext =
            { flags = flags
            , route = route
            , currentUrl = url
            , navKey = navKey
            }
    in
    ( Initializing preAppContext, Task.attempt FetchPreReqsFinished (fetchPreReqs preAppContext) )


type Msg
    = AppMsg App.Msg
    | FetchPreReqsFinished (HttpResult ( Time.Posix, Time.Zone, Session ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( Initializing preAppContext, FetchPreReqsFinished (Ok ( now, timeZone, session )) ) ->
            let
                appContext =
                    AppContext.init
                        preAppContext.flags
                        preAppContext.navKey
                        preAppContext.currentUrl
                        (DateTime.fromPosix now)
                        timeZone
                        session

                ( app, cmd ) =
                    App.init appContext preAppContext.route
            in
            ( Initialized app, Cmd.map AppMsg cmd )

        ( Initializing preAppContext, FetchPreReqsFinished (Err e) ) ->
            ( InitializationError preAppContext e, Cmd.none )

        ( _, AppMsg appMsg ) ->
            case model of
                Initialized a ->
                    let
                        ( app, cmd ) =
                            App.update appMsg a
                    in
                    ( Initialized app, Cmd.map AppMsg cmd )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


fetchPreReqs : PreAppContext -> Task Http.Error ( Time.Posix, Time.Zone, Session )
fetchPreReqs preAppContext =
    Task.map3
        (\n z s -> ( n, z, s ))
        Time.now
        Time.here
        (fetchSession preAppContext)


fetchSession : PreAppContext -> Task Http.Error Session
fetchSession preAppContext =
    let
        onError e =
            case e of
                Http.BadStatus 401 ->
                    Task.succeed Session.Anonymous

                Http.BadStatus 404 ->
                    Task.succeed Session.Anonymous

                _ ->
                    Task.fail e

        apiUrl =
            HttpApi.apiUrlFromString True preAppContext.flags.apiUrl
    in
    HttpApi.toTask apiUrl Session.decode ShareApi.session
        |> Task.onError onError


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Initialized app ->
            Sub.map AppMsg (App.subscriptions app)

        _ ->
            Sub.none


viewAppLoading : Html msg
viewAppLoading =
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                PageContent.empty
                PageFooter.pageFooter
            )
        ]


viewAppError : Http.Error -> Html msg
viewAppError error =
    let
        -- TODO: Better error
        ( errorTitle, _ ) =
            case error of
                Http.Timeout ->
                    ( "Unison Share took too long to respond.", "" )

                Http.NetworkError ->
                    ( "Unison Share is unreachable.", "" )

                Http.BadUrl _ ->
                    ( "Unison Share is unavailable.", "AN error occurred on our end." )

                Http.BadStatus _ ->
                    ( "Unison Share is unavailable.", "An error occurred on our end." )

                Http.BadBody _ ->
                    ( "Unison Share is unavailable.", "An error occurred on our end." )
    in
    div [ id "app" ]
        [ AppHeader.viewBlank
        , PageLayout.view
            (PageLayout.centeredLayout
                (PageContent.oneColumn
                    [ div [ class "app-error" ]
                        [ Icon.view Icon.warn
                        , p [ title (Util.httpErrorToString error) ]
                            [ text errorTitle ]
                        ]
                    ]
                )
                PageFooter.pageFooter
            )
        ]


view : Model -> Browser.Document Msg
view model =
    case model of
        Initializing _ ->
            { title = "Loading.. | Unison Share"
            , body = [ viewAppLoading ]
            }

        InitializationError _ error ->
            { title = "Application Error | Unison Share"
            , body = [ viewAppError error ]
            }

        Initialized appModel ->
            let
                app =
                    App.view appModel
            in
            { title = app.title
            , body = List.map (Html.map AppMsg) app.body
            }
