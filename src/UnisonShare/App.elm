module UnisonShare.App exposing (..)

import Browser
import Browser.Navigation as Nav
import Html
    exposing
        ( Html
        , aside
        , br
        , div
        , footer
        , h2
        , h3
        , header
        , li
        , p
        , section
        , span
        , text
        , ul
        )
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.OperatingSystem exposing (OperatingSystem(..))
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util as Util
import Time
import UI
import UI.Avatar as Avatar
import UI.Button as Button
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.KeyboardShortcut as KeyboardShortcut
import UI.KeyboardShortcut.Key as Key exposing (Key(..))
import UI.Modal as Modal
import UnisonShare.Account as Account exposing (Account)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument as AppDocument
import UnisonShare.AppError as AppError exposing (AppError)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.NewOrgModal as NewOrgModal
import UnisonShare.Page.AcceptTermsPage as AcceptTermsPage
import UnisonShare.Page.AccountPage as AccountPage
import UnisonShare.Page.AppErrorPage as AppErrorPage
import UnisonShare.Page.CatalogPage as CatalogPage
import UnisonShare.Page.CloudPage as CloudPage
import UnisonShare.Page.FinishSignupPage as FinishSignupPage
import UnisonShare.Page.NotFoundPage as NotFoundPage
import UnisonShare.Page.NotificationsPage as NotificationsPage
import UnisonShare.Page.OrgPage as OrgPage
import UnisonShare.Page.PrivacyPolicyPage as PrivacyPolicyPage
import UnisonShare.Page.ProfilePage as ProfilePage
import UnisonShare.Page.ProjectPage as ProjectPage
import UnisonShare.Page.TermsOfServicePage as TermsOfServicePage
import UnisonShare.Page.UcmConnectedPage as UcmConnectedPage
import UnisonShare.Page.UserPage as UserPage
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route exposing (Route)
import UnisonShare.Session as Session
import UnisonShare.Tour as Tour
import Url exposing (Url)
import Url.Parser as Parser
import Url.Parser.Query as Query
import WhatsNew exposing (WhatsNew)



-- MODEL


type Page
    = Catalog CatalogPage.Model
    | Account AccountPage.Model
    | Notifications Route.NotificationsRoute NotificationsPage.Model
    | Profile UserHandle ProfilePage.Model
    | User UserHandle Route.UserRoute UserPage.Model
    | Org UserHandle Route.OrgRoute OrgPage.Model
    | Project ProjectRef Route.ProjectRoute ProjectPage.Model
    | TermsOfService
    | AcceptTerms (Maybe Url) AcceptTermsPage.Model
    | PrivacyPolicy
    | UcmConnected
    | FinishSignup UserHandle FinishSignupPage.Model
    | Cloud
    | Error AppError
    | NotFound


type AppModal
    = NoModal
    | KeyboardShortcuts
    | NewOrg NewOrgModal.Model


type alias Model =
    { page : Page
    , appContext : AppContext
    , openedAppHeaderMenu : AppHeader.OpenedAppHeaderMenu
    , appModal : AppModal
    , whatsNew : WhatsNew
    }


init : AppContext -> Route -> ( Model, Cmd Msg )
init appContext route =
    let
        ( page, cmd ) =
            case route of
                Route.Catalog ->
                    let
                        url =
                            appContext.currentUrl

                        search =
                            Parser.parse (Parser.query (Query.string "search")) url
                                |> Maybe.withDefault Nothing
                                -- When using the Browser's address bar to
                                -- perform a search on Share it uses + instead
                                -- of %20 to encode spaces and this ends up as
                                -- literal + when parsed with Url.percentDecode
                                |> Maybe.map (String.replace "+" "%20")
                                |> Maybe.andThen Url.percentDecode

                        filter =
                            Parser.parse (Parser.query (Query.string "filter")) url
                                |> Maybe.withDefault Nothing
                                |> Maybe.andThen Url.percentDecode

                        ( catalog, catalogCmd ) =
                            CatalogPage.init appContext search filter
                    in
                    ( Catalog catalog, Cmd.map CatalogPageMsg catalogCmd )

                Route.Account ->
                    let
                        account =
                            AccountPage.init
                    in
                    ( Account account, Cmd.none )

                Route.Notifications r ->
                    case appContext.session of
                        Session.SignedIn account ->
                            let
                                ( notifications, notificationsCmd ) =
                                    NotificationsPage.init appContext r account
                            in
                            ( Notifications r notifications, Cmd.map NotificationsPageMsg notificationsCmd )

                        Session.Anonymous ->
                            ( NotFound, Cmd.none )

                Route.Profile handle ->
                    let
                        ( profile, profileCmd ) =
                            ProfilePage.init appContext handle
                    in
                    ( Profile handle profile, Cmd.map ProfilePageMsg profileCmd )

                Route.User handle userRoute ->
                    let
                        ( user, userCmd ) =
                            UserPage.init appContext handle userRoute
                    in
                    ( User handle userRoute user, Cmd.map UserPageMsg userCmd )

                Route.Org handle orgRoute ->
                    let
                        ( org, orgCmd ) =
                            OrgPage.init appContext handle orgRoute
                    in
                    ( Org handle orgRoute org, Cmd.map OrgPageMsg orgCmd )

                Route.Project projectRef projectRoute ->
                    let
                        ( project, userCmd ) =
                            ProjectPage.init appContext projectRef projectRoute
                    in
                    ( Project projectRef projectRoute project, Cmd.map ProjectPageMsg userCmd )

                Route.TermsOfService ->
                    ( TermsOfService, Cmd.none )

                Route.AcceptTerms continueUrl ->
                    ( AcceptTerms continueUrl AcceptTermsPage.init, Cmd.none )

                Route.PrivacyPolicy ->
                    ( PrivacyPolicy, Cmd.none )

                Route.UcmConnected ->
                    ( UcmConnected, Cmd.none )

                Route.FinishSignup handle _ ->
                    ( FinishSignup handle FinishSignupPage.init, Cmd.none )

                Route.Cloud ->
                    ( Cloud, Cmd.none )

                Route.Error e ->
                    ( Error e, Cmd.none )

                Route.NotFound _ ->
                    ( NotFound, Cmd.none )

        model =
            { page = page
            , appContext = appContext
            , openedAppHeaderMenu = AppHeader.NoneOpened
            , appModal = NoModal
            , whatsNew = WhatsNew.Loading (List.map WhatsNew.PostId appContext.whatsNewReadPostIds)
            }
    in
    ( model
    , Cmd.batch
        [ cmd
        , WhatsNew.fetchFeed appContext model.whatsNew WhatsNewFetchFinished
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | Tick Time.Posix
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | AcceptWelcomeTerms
    | ToggleHelpAndResourcesMenu
    | ToggleAccountMenu
    | ToggleCreateAccountMenu
    | ShowKeyboardShortcuts
    | ShowNewOrgModal
    | CloseModal
    | WhatsNewFetchFinished (HttpResult WhatsNew.LoadedWhatsNew)
    | WhatsNewMarkAllAsRead
    | RefreshSessionFinished (HttpResult Session.Session)
      -- Sub msgs
    | CatalogPageMsg CatalogPage.Msg
    | ProfilePageMsg ProfilePage.Msg
    | UserPageMsg UserPage.Msg
    | OrgPageMsg OrgPage.Msg
    | ProjectPageMsg ProjectPage.Msg
    | AccountPageMsg AccountPage.Msg
    | NotificationsPageMsg NotificationsPage.Msg
    | AcceptTermsPageMsg AcceptTermsPage.Msg
    | FinishSignupPageMsg FinishSignupPage.Msg
    | NewOrgModalMsg NewOrgModal.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ appContext } as model) =
    case ( model.page, msg ) of
        ( _, Tick t ) ->
            let
                appContext_ =
                    { appContext | now = DateTime.fromPosix t }
            in
            ( { model | appContext = appContext_ }, Cmd.none )

        ( _, RefreshSessionFinished session ) ->
            case session of
                Ok session_ ->
                    let
                        appContext_ =
                            { appContext | session = session_ }
                    in
                    ( { model | appContext = appContext_ }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl appContext.navKey (Url.toString url) )

                -- External links are handled via target blank and never end up
                -- here except for login and logout
                Browser.External url ->
                    if String.contains "logout" url || String.contains "login" url then
                        ( model, Nav.load url )

                    else
                        ( model, Cmd.none )

        ( _, UrlChanged url ) ->
            let
                route =
                    Route.fromUrl appContext.basePath url

                appContext_ =
                    { appContext | currentUrl = url }

                model_ =
                    { model | appContext = appContext_ }

                ( m, c ) =
                    case route of
                        Route.Catalog ->
                            case model.page of
                                Catalog _ ->
                                    -- The URL changed because we added to the search
                                    -- query parameter. This doesn't warrant
                                    -- re-initializing the CatalogPage
                                    ( model_, Cmd.none )

                                _ ->
                                    let
                                        ( catalog, cmd ) =
                                            CatalogPage.init appContext_ Nothing Nothing
                                    in
                                    ( { model_ | page = Catalog catalog }, Cmd.map CatalogPageMsg cmd )

                        Route.Account ->
                            ( { model_ | page = Account AccountPage.init }, Cmd.none )

                        Route.Notifications r ->
                            case appContext.session of
                                Session.SignedIn account ->
                                    let
                                        ( notifications, cmd ) =
                                            NotificationsPage.init appContext_ r account
                                    in
                                    ( { model_ | page = Notifications r notifications }, Cmd.map NotificationsPageMsg cmd )

                                Session.Anonymous ->
                                    ( { model | page = NotFound }, Cmd.none )

                        Route.Profile handle ->
                            let
                                ( profile, cmd ) =
                                    ProfilePage.init appContext_ handle
                            in
                            ( { model_ | page = Profile handle profile }, Cmd.map ProfilePageMsg cmd )

                        Route.User handle userRoute ->
                            case model_.page of
                                User currentHandle _ userModel ->
                                    if UserHandle.equals currentHandle handle then
                                        let
                                            ( user, cmd ) =
                                                UserPage.updateSubPage appContext_ handle userModel userRoute
                                        in
                                        ( { model_ | page = User handle userRoute user }, Cmd.map UserPageMsg cmd )

                                    else
                                        let
                                            ( user, cmd ) =
                                                UserPage.init appContext_ handle userRoute
                                        in
                                        ( { model_ | page = User handle userRoute user }, Cmd.map UserPageMsg cmd )

                                _ ->
                                    let
                                        ( user, cmd ) =
                                            UserPage.init appContext_ handle userRoute
                                    in
                                    ( { model_ | page = User handle userRoute user }, Cmd.map UserPageMsg cmd )

                        Route.Org handle orgRoute ->
                            case model_.page of
                                Org currentHandle _ userModel ->
                                    if UserHandle.equals currentHandle handle then
                                        let
                                            ( org, cmd ) =
                                                OrgPage.updateSubPage appContext_ handle userModel orgRoute
                                        in
                                        ( { model_ | page = Org handle orgRoute org }, Cmd.map OrgPageMsg cmd )

                                    else
                                        let
                                            ( org, cmd ) =
                                                OrgPage.init appContext_ handle orgRoute
                                        in
                                        ( { model_ | page = Org handle orgRoute org }, Cmd.map OrgPageMsg cmd )

                                _ ->
                                    let
                                        ( org, cmd ) =
                                            OrgPage.init appContext_ handle orgRoute
                                    in
                                    ( { model_ | page = Org handle orgRoute org }, Cmd.map OrgPageMsg cmd )

                        Route.Project projectRef projectRoute ->
                            case model_.page of
                                Project currentProjectRef _ projectModel ->
                                    if ProjectRef.equals currentProjectRef projectRef then
                                        let
                                            ( project, cmd ) =
                                                ProjectPage.updateSubPage appContext_ projectRef projectModel projectRoute
                                        in
                                        ( { model_ | page = Project projectRef projectRoute project }, Cmd.map ProjectPageMsg cmd )

                                    else
                                        let
                                            ( project, cmd ) =
                                                ProjectPage.init appContext_ projectRef projectRoute
                                        in
                                        ( { model_ | page = Project projectRef projectRoute project }, Cmd.map ProjectPageMsg cmd )

                                _ ->
                                    let
                                        ( project, cmd ) =
                                            ProjectPage.init appContext_ projectRef projectRoute
                                    in
                                    ( { model_ | page = Project projectRef projectRoute project }, Cmd.map ProjectPageMsg cmd )

                        Route.TermsOfService ->
                            ( { model_ | page = TermsOfService }, Cmd.none )

                        Route.AcceptTerms continueUrl ->
                            ( { model_ | page = AcceptTerms continueUrl AcceptTermsPage.init }, Cmd.none )

                        Route.PrivacyPolicy ->
                            ( { model_ | page = PrivacyPolicy }, Cmd.none )

                        Route.UcmConnected ->
                            ( { model_ | page = UcmConnected }, Cmd.none )

                        Route.FinishSignup conflictingHandle _ ->
                            ( { model_ | page = FinishSignup conflictingHandle FinishSignupPage.init }, Cmd.none )

                        Route.Cloud ->
                            ( { model_ | page = Cloud }, Cmd.none )

                        Route.Error e ->
                            ( { model_ | page = Error e }, Cmd.none )

                        Route.NotFound _ ->
                            ( { model_ | page = NotFound }, Cmd.none )
            in
            ( m, c )

        ( _, WhatsNewFetchFinished r ) ->
            let
                whatsNew =
                    case r of
                        Ok wn ->
                            WhatsNew.Success wn

                        Err _ ->
                            let
                                readPostIds =
                                    case model.whatsNew of
                                        WhatsNew.Loading ids ->
                                            ids

                                        WhatsNew.Success d ->
                                            d.readPostIds

                                        WhatsNew.Failure ids ->
                                            ids
                            in
                            WhatsNew.Failure readPostIds
            in
            ( { model | whatsNew = whatsNew }, Cmd.none )

        ( _, AcceptWelcomeTerms ) ->
            case appContext.session of
                Session.SignedIn a ->
                    ( { model
                        | appContext =
                            { appContext
                                | session = Session.SignedIn (Account.markTourAsCompleted Tour.WelcomeTerms a)
                            }
                      }
                    , completeWelcomeTour appContext
                    )

                Session.Anonymous ->
                    ( model, Cmd.none )

        ( _, WhatsNewMarkAllAsRead ) ->
            let
                ( wn, cmd ) =
                    WhatsNew.markAllAsRead model.whatsNew
            in
            ( { model | whatsNew = wn }, cmd )

        ( _, ToggleHelpAndResourcesMenu ) ->
            let
                ( openedAppHeaderMenu, cmd ) =
                    if model.openedAppHeaderMenu == AppHeader.HelpAndResourcesMenu then
                        ( AppHeader.NoneOpened, Cmd.none )

                    else
                        ( AppHeader.HelpAndResourcesMenu, Util.delayMsg 2500 WhatsNewMarkAllAsRead )
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, cmd )

        ( _, ToggleAccountMenu ) ->
            let
                openedAppHeaderMenu =
                    if model.openedAppHeaderMenu == AppHeader.AccountMenu then
                        AppHeader.NoneOpened

                    else
                        AppHeader.AccountMenu
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, Cmd.none )

        ( _, ToggleCreateAccountMenu ) ->
            let
                openedAppHeaderMenu =
                    if model.openedAppHeaderMenu == AppHeader.CreateAccountMenu then
                        AppHeader.NoneOpened

                    else
                        AppHeader.CreateAccountMenu
            in
            ( { model | openedAppHeaderMenu = openedAppHeaderMenu }, Cmd.none )

        ( _, ShowKeyboardShortcuts ) ->
            ( { model | openedAppHeaderMenu = AppHeader.NoneOpened, appModal = KeyboardShortcuts }, Cmd.none )

        ( _, ShowNewOrgModal ) ->
            ( { model | openedAppHeaderMenu = AppHeader.NoneOpened, appModal = NewOrg NewOrgModal.init }, Cmd.none )

        ( _, CloseModal ) ->
            ( { model | appModal = NoModal }, Cmd.none )

        -- Sub msgs
        ( Catalog m, CatalogPageMsg cMsg ) ->
            let
                ( catalog, cmd ) =
                    CatalogPage.update appContext cMsg m
            in
            ( { model | page = Catalog catalog }, Cmd.map CatalogPageMsg cmd )

        ( Profile handle m, ProfilePageMsg pMsg ) ->
            let
                ( profile, cmd ) =
                    ProfilePage.update appContext handle pMsg m
            in
            ( { model | page = Profile handle profile }, Cmd.map ProfilePageMsg cmd )

        ( User handle route m, UserPageMsg uMsg ) ->
            let
                ( user, cmd ) =
                    UserPage.update appContext handle route uMsg m
            in
            ( { model | page = User handle route user }, Cmd.map UserPageMsg cmd )

        ( Org handle route m, OrgPageMsg oMsg ) ->
            let
                ( org, cmd ) =
                    OrgPage.update appContext handle route oMsg m
            in
            ( { model | page = Org handle route org }, Cmd.map OrgPageMsg cmd )

        ( Project projectRef route m, ProjectPageMsg pMsg ) ->
            let
                ( project, cmd ) =
                    ProjectPage.update appContext projectRef route pMsg m
            in
            ( { model | page = Project projectRef route project }, Cmd.map ProjectPageMsg cmd )

        ( Account m, AccountPageMsg aMsg ) ->
            case appContext.session of
                Session.SignedIn a ->
                    let
                        ( accountM, cmd ) =
                            AccountPage.update appContext a aMsg m
                    in
                    ( { model | page = Account accountM }, Cmd.map AccountPageMsg cmd )

                _ ->
                    ( model, Cmd.none )

        ( Notifications notificationsRoute notifications, NotificationsPageMsg nMsg ) ->
            case appContext.session of
                Session.SignedIn account ->
                    let
                        ( notifications_, cmd, out ) =
                            NotificationsPage.update appContext notificationsRoute account nMsg notifications

                        sessionCmd =
                            case out of
                                NotificationsPage.NoOutMsg ->
                                    Cmd.none

                                NotificationsPage.UpdatedNotificationStatuses ->
                                    refreshSession appContext
                    in
                    ( { model | page = Notifications notificationsRoute notifications_ }
                    , Cmd.batch [ Cmd.map NotificationsPageMsg cmd, sessionCmd ]
                    )

                _ ->
                    ( model, Cmd.none )

        ( AcceptTerms continueUrl acceptTerms, AcceptTermsPageMsg atMsg ) ->
            let
                ( acceptTerms_, acceptTermsCmd ) =
                    AcceptTermsPage.update appContext atMsg continueUrl acceptTerms
            in
            ( { model | page = AcceptTerms continueUrl acceptTerms_ }, Cmd.map AcceptTermsPageMsg acceptTermsCmd )

        ( FinishSignup conflictingHandle finishSignup, FinishSignupPageMsg fsMsg ) ->
            let
                ( finishSignup_, finishSignupCmd ) =
                    FinishSignupPage.update appContext conflictingHandle fsMsg finishSignup
            in
            ( { model | page = FinishSignup conflictingHandle finishSignup_ }, Cmd.map FinishSignupPageMsg finishSignupCmd )

        ( _, NewOrgModalMsg newOrgMsg ) ->
            case ( model.appModal, appContext.session ) of
                ( NewOrg newOrg, Session.SignedIn account ) ->
                    let
                        ( newOrg_, cmd, out ) =
                            NewOrgModal.update appContext account newOrgMsg newOrg

                        ( appModal, navCmd ) =
                            case out of
                                NewOrgModal.NoOutMsg ->
                                    ( NewOrg newOrg_, Cmd.none )

                                NewOrgModal.RequestCloseModal ->
                                    ( NoModal, Cmd.none )

                                NewOrgModal.AddedOrg addedOrg ->
                                    ( NoModal, Route.navigate appContext.navKey (Route.orgProfile addedOrg.handle) )
                    in
                    ( { model | appModal = appModal }, Cmd.batch [ Cmd.map NewOrgModalMsg cmd, navCmd ] )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- EFFECTS


completeWelcomeTour : AppContext -> Cmd Msg
completeWelcomeTour appContext =
    ShareApi.completeTours [ Tour.WelcomeTerms ]
        |> HttpApi.toRequestWithEmptyResponse (\_ -> NoOp)
        |> HttpApi.perform appContext.api


refreshSession : AppContext -> Cmd Msg
refreshSession appContext =
    ShareApi.session
        |> HttpApi.toRequest Session.decode RefreshSessionFinished
        |> HttpApi.perform appContext.api



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        sub =
            case model.page of
                Project _ _ pp ->
                    Sub.map ProjectPageMsg (ProjectPage.subscriptions pp)

                _ ->
                    Sub.none
    in
    Sub.batch
        [ sub

        -- Keep track of the current time for things like "2 minutes ago"
        , Time.every 1000 Tick
        ]



-- VIEW


viewKeyboardShortcutsModal : OperatingSystem -> Html Msg
viewKeyboardShortcutsModal os =
    let
        -- The shortcut views requires a model, but we don't really need it for this kind of overview
        keyboardShortcut =
            KeyboardShortcut.init os

        viewRow label instructions =
            div
                [ class "row" ]
                [ label
                , div [ class "instructions" ] instructions
                ]

        viewInstructions label shortcuts =
            viewRow label [ KeyboardShortcut.viewShortcuts keyboardShortcut shortcuts ]

        openFinderInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (K Key.Lower), KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (K Key.Lower), KeyboardShortcut.single ForwardSlash ]

        toggleSidebarInstructions =
            case os of
                MacOS ->
                    [ KeyboardShortcut.Chord Meta (B Key.Lower), KeyboardShortcut.Chord Ctrl (B Key.Lower) ]

                _ ->
                    [ KeyboardShortcut.Chord Ctrl (B Key.Lower) ]

        content =
            Modal.Content
                (section
                    [ class "shortcuts" ]
                    [ div [ class "shortcut-group" ]
                        [ h3 [] [ text "Browsing Code" ]
                        , viewInstructions (text "Open Finder") openFinderInstructions
                        , viewInstructions (text "Toggle sidebar") toggleSidebarInstructions
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp, KeyboardShortcut.single (K Key.Lower) ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown, KeyboardShortcut.single (J Key.Lower) ]
                        , viewInstructions (text "Close focused definition") [ KeyboardShortcut.single (X Key.Lower) ]
                        , viewInstructions (text "Expand/Collapse focused definition") [ KeyboardShortcut.single Space ]
                        ]
                    , div [ class "shortcut-group" ]
                        [ h3 [] [ text "Finder" ]
                        , viewInstructions (text "Clear search query") [ KeyboardShortcut.single Escape ]
                        , viewInstructions (span [] [ text "Close", UI.subtle " (when search query is empty)" ]) [ KeyboardShortcut.single Escape ]
                        , viewInstructions (text "Move focus up") [ KeyboardShortcut.single ArrowUp ]
                        , viewInstructions (text "Move focus down") [ KeyboardShortcut.single ArrowDown ]
                        , viewInstructions (text "Open focused definition") [ KeyboardShortcut.single Enter ]
                        , viewRow (text "Open definition")
                            [ KeyboardShortcut.viewBase
                                [ KeyboardShortcut.viewKey os Semicolon False
                                , KeyboardShortcut.viewThen
                                , KeyboardShortcut.viewKeyBase "1-9" False
                                ]
                            ]
                        ]
                    ]
                )
    in
    Modal.modal "help-modal" CloseModal content
        |> Modal.withHeader "Keyboard shortcuts"
        |> Modal.view


viewWelcomeTermsModal : Account a -> Html Msg
viewWelcomeTermsModal account =
    let
        avatar =
            Account.toAvatar account |> Avatar.view

        content =
            div []
                [ header [ class "welcome" ]
                    [ div [ class "avatar-container" ] [ avatar ]
                    , div []
                        [ h2 [] [ Icon.view Icon.tada, text "Welcome" ]
                        , p []
                            [ text
                                ("Hello " ++ Account.name account ++ ", and welcome to Unison Share!")
                            , br [] []
                            , text "We're really excited that you are here and can't wait to see what you do with Unison."
                            ]
                        , p [] [ text "If you haven’t already, consider joining our", Button.iconThenLabel_ Link.discord Icon.discord "Discord Community" |> Button.small |> Button.view ]
                        ]
                    ]
                , UI.divider
                , div []
                    [ h2 [] [ Icon.view Icon.star, text "Membership Tenets" ]
                    , div [ class "membership-tenets" ]
                        [ div [ class "tenets" ]
                            [ p []
                                [ text "Share is not just a code hosting service, it’s also a place for the Unison community to learn from one another, collaborate, and connect."
                                ]
                            , p []
                                [ text "Membership here means you are an important steward of our community. In addition to the "
                                , Link.view "Code of Conduct" Link.codeOfConduct
                                , text " and "
                                , Link.view "Terms of Service" Link.termsOfService
                                , text ", you can help us create an inclusive, welcoming space by:"
                                ]
                            , ul []
                                [ li [] [ Icon.view Icon.dot, text "Sharing knowledge and encouragement generously 🎁" ]
                                , li [] [ Icon.view Icon.dot, text "Being kind to each other 💖" ]
                                , li [] [ Icon.view Icon.dot, text "Being respectful and empathetic when disagreeing 🤝" ]
                                , li [] [ Icon.view Icon.dot, text "Supporting people of all skill levels 🐣" ]
                                , li [] [ Icon.view Icon.dot, text "Keeping our community free of discrimination, contempt, or harassment ☮️" ]
                                ]
                            ]
                        , aside [ class "documents" ]
                            [ p [] [ text "By using Unison Share you agree to the following" ]
                            , ul []
                                [ li [] [ Icon.view Icon.heart, Link.view "Code of Conduct" Link.codeOfConduct ]
                                , li [] [ Icon.view Icon.certificate, Link.view "Terms of Service" Link.termsOfService ]
                                , li [] [ Icon.view Icon.eyeSlash, Link.view "Privacy Policy" Link.privacyPolicy ]
                                ]
                            ]
                        ]
                    ]
                , footer []
                    [ Button.iconThenLabel AcceptWelcomeTerms Icon.checkmark "Accept and Continue to Unison Share"
                        |> Button.positive
                        |> Button.medium
                        |> Button.view
                    ]
                ]
    in
    Modal.modal_
        "welcome-tour-modal"
        (Modal.Content content)
        |> Modal.view


view : Model -> Browser.Document Msg
view model =
    let
        appContext =
            model.appContext

        session =
            appContext.session

        appHeaderContext =
            { session = session
            , timeZone = appContext.timeZone
            , currentUrl = appContext.currentUrl
            , whatsNew = model.whatsNew
            , api = appContext.api
            , openedAppHeaderMenu = model.openedAppHeaderMenu
            , toggleHelpAndResourcesMenuMsg = ToggleHelpAndResourcesMenu
            , toggleAccountMenuMsg = ToggleAccountMenu
            , toggleCreateAccountMenuMsg = ToggleCreateAccountMenu
            , showKeyboardShortcutsModalMsg = ShowKeyboardShortcuts
            , showNewOrgModal = ShowNewOrgModal
            }

        appDocument =
            case model.page of
                Catalog catalog ->
                    AppDocument.map CatalogPageMsg (CatalogPage.view appContext catalog)

                Account accountModel ->
                    case session of
                        Session.SignedIn a ->
                            AppDocument.map AccountPageMsg (AccountPage.view a accountModel)

                        Session.Anonymous ->
                            NotFoundPage.view

                Notifications _ notifications ->
                    AppDocument.map NotificationsPageMsg (NotificationsPage.view appContext notifications)

                Profile _ profile ->
                    AppDocument.map ProfilePageMsg (ProfilePage.view appContext profile)

                User handle _ userModel ->
                    AppDocument.map UserPageMsg (UserPage.view appContext handle userModel)

                Org handle _ orgModel ->
                    AppDocument.map OrgPageMsg (OrgPage.view appContext handle orgModel)

                Project projectRef _ projectModel ->
                    AppDocument.map ProjectPageMsg (ProjectPage.view appContext projectRef projectModel)

                TermsOfService ->
                    TermsOfServicePage.view

                AcceptTerms _ acceptTerms ->
                    acceptTerms
                        |> AcceptTermsPage.view
                        |> AppDocument.map AcceptTermsPageMsg

                PrivacyPolicy ->
                    PrivacyPolicyPage.view

                UcmConnected ->
                    case appContext.session of
                        Session.Anonymous ->
                            AppErrorPage.view appContext AppError.UnspecifiedError

                        Session.SignedIn a ->
                            UcmConnectedPage.view a

                FinishSignup conflictingHandle finishSignup ->
                    case appContext.session of
                        Session.Anonymous ->
                            finishSignup
                                |> FinishSignupPage.view appContext conflictingHandle
                                |> AppDocument.map FinishSignupPageMsg

                        Session.SignedIn _ ->
                            NotFoundPage.view

                Cloud ->
                    CloudPage.view session

                Error error ->
                    AppErrorPage.view appContext error

                NotFound ->
                    NotFoundPage.view

        -- Overwrite Modal with any app level, user initiated modal
        appDocumentWithModal =
            case model.appModal of
                NoModal ->
                    appDocument

                KeyboardShortcuts ->
                    { appDocument | modal = Just (viewKeyboardShortcutsModal appContext.operatingSystem) }

                NewOrg m ->
                    { appDocument | modal = Just (Html.map NewOrgModalMsg (NewOrgModal.view m)) }

        appDocumentWithWelcomeTermsModal =
            -- We link to TermsOfService and PrivacyPolicy from the welcome
            -- terms modal and the AcceptTerms page is used during UCM signup,
            -- so we don't want to block those pages by the welcome modal.
            case ( model.page, model.appContext.session ) of
                ( TermsOfService, _ ) ->
                    appDocumentWithModal

                ( AcceptTerms _ _, _ ) ->
                    appDocumentWithModal

                ( PrivacyPolicy, _ ) ->
                    appDocumentWithModal

                ( _, Session.SignedIn a ) ->
                    if not (Account.hasCompletedTour Tour.WelcomeTerms a) then
                        { appDocumentWithModal
                            | modal =
                                Just (viewWelcomeTermsModal a)
                        }

                    else
                        appDocumentWithModal

                _ ->
                    appDocumentWithModal
    in
    AppDocument.view appHeaderContext appDocumentWithWelcomeTermsModal
