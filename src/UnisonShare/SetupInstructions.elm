module UnisonShare.SetupInstructions exposing (..)

import Code.Perspective as Perspective
import Html exposing (Html, div, li, ol, p, span, strong, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.OperatingSystem as OS exposing (OperatingSystem)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.CopyField as CopyField
import UI.Icon as Icon
import UI.StatusBanner as StatusBanner
import UI.Steps as Steps
import UnisonShare.Account exposing (Account)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.CodeBrowsingContext as CodeBrowsingContext
import UnisonShare.Link as Link
import UnisonShare.UnisonRelease as UnisonRelease



-- MODEL


type alias Model =
    { isEmptyCodebase : WebData Bool }


init : AppContext -> Account a -> ( Model, Cmd Msg )
init appContext account =
    ( { isEmptyCodebase = Success True }
    , checkCodebaseEmptiness appContext account.handle
    )



-- UPDATE


type Msg
    = NoOp
    | CheckCodebaseEmptiness
    | EmptinessCheckFinished (HttpResult Bool)
    | Done


type OutMsg
    = Remain
    | NoLongerEmpty
    | Exit


update : AppContext -> Account a -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext account msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, Remain )

        CheckCodebaseEmptiness ->
            ( model, checkCodebaseEmptiness appContext account.handle, Remain )

        EmptinessCheckFinished (Ok isEmpty) ->
            -- If the codebase is still empty, schedule another check in 5s
            if isEmpty then
                ( model, Util.delayMsg 5000 CheckCodebaseEmptiness, Remain )

            else
                -- If we're done, update the model and exit after 10s so that we can show the success message briefly.
                ( { model | isEmptyCodebase = Success False }, Util.delayMsg 5000 Done, NoLongerEmpty )

        EmptinessCheckFinished (Err _) ->
            -- Somehow the check failed, lets try again in 10s
            ( model, Util.delayMsg 10000 CheckCodebaseEmptiness, Remain )

        Done ->
            ( model, Cmd.none, Exit )



-- EFFECTS


checkCodebaseEmptiness : AppContext -> UserHandle -> Cmd Msg
checkCodebaseEmptiness appContext handle =
    let
        decoder =
            -- Parse to "not-empty" since we don't actually care about the values, just that they exist
            Decode.map List.isEmpty (Decode.field "namespaceListingChildren" (Decode.list (Decode.succeed "not-empty")))
    in
    ShareApi.browseCodebase
        (CodeBrowsingContext.UserCode handle)
        Perspective.relativeRootPerspective
        Nothing
        |> HttpApi.toRequest decoder EmptinessCheckFinished
        |> HttpApi.perform appContext.api



-- VIEW


installUcmStep : OperatingSystem -> Steps.Step Msg
installUcmStep os =
    let
        release =
            UnisonRelease.latest

        macCommand =
            UnisonRelease.installForMac release

        linuxCommands =
            UnisonRelease.installForLinux release |> String.join "\n"

        install =
            case os of
                OS.MacOS ->
                    CopyField.copyField (\_ -> NoOp) macCommand |> CopyField.withPrefix "$" |> CopyField.view

                OS.Linux ->
                    div [ class "install-instructions" ]
                        [ strong [] [ text "To install on Linux, run these commands:" ]
                        , UI.codeBlock [ class "code" ] (text linuxCommands)
                        ]

                OS.Windows ->
                    ol []
                        [ li []
                            [ text "Set your default terminal application to \"Windows Terminal\" for best results. Search for \"Terminal\" in Settings, or follow this "
                            , Link.view "how-to." (Link.link "https://www.tenforums.com/tutorials/180053-how-change-default-terminal-application-windows-10-a.html")
                            ]
                        , li []
                            [ text "Download "
                            , Link.view "UCM" (Link.link (UnisonRelease.windowsUrl release))
                            , text " and extract it to a location of your choosing."
                            ]
                        , li [] [ text "Run ", UI.inlineCode [] (text "ucm.exe") ]
                        ]

                _ ->
                    strong []
                        [ text "Download the latest build from GitHub releases: "
                        , Link.githubRelease release.tag |> Link.view release.name
                        ]
    in
    Steps.step "Install UCM"
        [ p []
            [ text "The Unison Codebase Manager, or UCM for short, is the main tool for writing Unison programs. Its an interactive CLI that provides access to your local codebase, helps you navigate it, and typechecks your code." ]
        , install
        , p [ class "browser-hint" ]
            [ Icon.view Icon.bulb
            , span []
                [ text "Learn more about UCM in the "
                , Link.view "Unison Tour" Link.tour
                , text "."
                ]
            ]
        ]


pushScratchStep : UserHandle -> Steps.Step Msg
pushScratchStep handle_ =
    let
        handle =
            UserHandle.toUnprefixedString handle_

        exampleCode =
            ".yourCode"

        remoteLocation =
            handle ++ ".public" ++ exampleCode

        pushCommand =
            "push.create " ++ remoteLocation ++ " " ++ exampleCode
    in
    Steps.step "Push code to Unison Share"
        [ p []
            [ text ("Unison Share hosts your code in a public namespace under your handle (@" ++ handle ++ "), an example namespace structure of a local project might look like this:")
            ]
        , div [ class "example-namespace-structure" ]
            [ UI.inlineCode [] (text remoteLocation)
            ]
        , p [] [ text "While running UCM, push your local code your Unison Share codebase (this page) with this UCM command:" ]
        , CopyField.copyField (\_ -> NoOp) pushCommand |> CopyField.withPrefix ".>" |> CopyField.view
        , div [ class "browser-hint" ]
            [ Icon.view Icon.bulb
            , div []
                [ p []
                    [ text "This will push all the contents of "
                    , UI.inlineCode [] (text exampleCode)
                    , text " to "
                    , UI.inlineCode [] (text remoteLocation)
                    , text " in Unison Share."
                    ]
                , p [] [ text "When running this command, a new browser window might open to authenticate UCM with Unison Share." ]
                ]
            ]
        ]


view : AppContext -> Account a -> Model -> Html Msg
view appContext account model =
    let
        steps =
            Steps.steps (installUcmStep appContext.operatingSystem)
                [ pushScratchStep account.handle
                ]
    in
    case model.isEmptyCodebase of
        Success False ->
            div [ class "setup-instructions" ]
                [ StatusBanner.good "Sweet! You've successfully pushed code to Unison Share."
                ]

        _ ->
            let
                status =
                    StatusBanner.working "Waiting on your first push—Unison Share isn't much without your code"
            in
            div [ class "setup-instructions" ]
                [ status
                , UI.divider
                , Steps.view steps
                ]
