module UnisonShare.AppHeader exposing (..)

import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, classList)
import Lib.HttpApi exposing (HttpApi)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Time
import UI
import UI.ActionMenu as ActionMenu
import UI.AnchoredOverlay as AnchoredOverlay
import UI.AppHeader exposing (AppHeader, AppTitle(..))
import UI.Avatar as Avatar
import UI.Button as Button
import UI.Click as Click exposing (Click)
import UI.DateTime as DateTime
import UI.Icon as Icon
import UI.Navigation as Navigation exposing (NavItem)
import UI.Nudge as Nudge
import UI.Sizing as Sizing
import UI.ViewMode as ViewMode exposing (ViewMode)
import UnisonShare.Link as Link
import UnisonShare.Session exposing (Session(..))
import Url exposing (Url)
import WhatsNew exposing (WhatsNew)


type AppHeader
    = Disabled ViewMode
    | AppHeader
        { activeNavItem : ActiveNavItem
        , viewMode : ViewMode
        }


type ActiveNavItem
    = None
    | Catalog
    | Profile



-- CREATE


empty : AppHeader
empty =
    Disabled ViewMode.Regular


appHeader : ActiveNavItem -> AppHeader
appHeader activeNavItem =
    empty |> withActiveNavItem activeNavItem



-- MODIFY


withViewMode : ViewMode -> AppHeader -> AppHeader
withViewMode vm appHeader_ =
    case appHeader_ of
        Disabled _ ->
            Disabled vm

        AppHeader a ->
            AppHeader { a | viewMode = vm }


withActiveNavItem : ActiveNavItem -> AppHeader -> AppHeader
withActiveNavItem activeNavItem appHeader_ =
    case appHeader_ of
        Disabled vm ->
            AppHeader
                { activeNavItem = activeNavItem
                , viewMode = vm
                }

        AppHeader a ->
            AppHeader { a | activeNavItem = activeNavItem }



-- VIEW


appTitle : Click msg -> AppTitle msg
appTitle click =
    AppTitle click
        (h1 []
            [ text "Unison"
            , span [ class "context unison-share" ] [ text "Share" ]
            ]
        )


navItems : { catalog : NavItem msg, profile : UserHandle -> NavItem msg }
navItems =
    { catalog = Navigation.navItem "Catalog" (Click.href "/")
    , profile =
        \h ->
            Navigation.navItem (UserHandle.toString h) (Link.userProfile h)
    }


{-| Represents app level context, that is injected at render time
-}
type OpenedAppHeaderMenu
    = NoneOpened
    | HelpAndResourcesMenu
    | AccountMenu
    | CreateAccountMenu


type alias AppHeaderContext msg =
    { session : Session
    , timeZone : Time.Zone
    , currentUrl : Url
    , api : HttpApi
    , openedAppHeaderMenu : OpenedAppHeaderMenu
    , whatsNew : WhatsNew
    , toggleHelpAndResourcesMenuMsg : msg
    , toggleAccountMenuMsg : msg
    , toggleCreateAccountMenuMsg : msg
    , showNewOrgModal : msg
    , showKeyboardShortcutsModalMsg : msg
    }


isHelpAndResourcesMenuOpen : OpenedAppHeaderMenu -> Bool
isHelpAndResourcesMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == HelpAndResourcesMenu


isAccountMenuOpen : OpenedAppHeaderMenu -> Bool
isAccountMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == AccountMenu


isCreateAccountMenuOpen : OpenedAppHeaderMenu -> Bool
isCreateAccountMenuOpen openedAppHeaderMenu =
    openedAppHeaderMenu == CreateAccountMenu


view : AppHeaderContext msg -> AppHeader -> Html msg
view ctx appHeader_ =
    case appHeader_ of
        Disabled viewMode ->
            viewBlank viewMode

        AppHeader { activeNavItem, viewMode } ->
            let
                whatsNewItemsLoading =
                    [ ActionMenu.loadingItem
                    , ActionMenu.loadingItem
                    , ActionMenu.loadingItem
                    , ActionMenu.loadingItem
                    ]

                whatsNewItems =
                    case ctx.whatsNew of
                        WhatsNew.Loading _ ->
                            whatsNewItemsLoading

                        WhatsNew.Success wn ->
                            let
                                viewPost p =
                                    let
                                        postNudge =
                                            if WhatsNew.isUnread wn p then
                                                Nudge.nudge

                                            else
                                                Nudge.empty
                                    in
                                    ActionMenu.optionItem_
                                        Nothing
                                        p.title
                                        (ActionMenu.DateTimeSubtext DateTime.ShortDate ctx.timeZone p.publishedAt)
                                        postNudge
                                        (Link.link p.url)
                            in
                            (wn.posts |> List.take 3 |> List.map viewPost)
                                ++ [ ActionMenu.optionItemWithoutIcon "View more..." Link.whatsNew
                                   ]

                        WhatsNew.Failure _ ->
                            []

                nudge =
                    if WhatsNew.hasAnyUnreadPosts ctx.whatsNew then
                        Nudge.nudge

                    else
                        Nudge.empty

                helpAndResources =
                    ActionMenu.items
                        (ActionMenu.titleItem "Whats new?")
                        (whatsNewItems
                            ++ [ ActionMenu.dividerItem
                               , ActionMenu.optionItem Icon.docs "Docs" Link.docs
                               , ActionMenu.optionItem Icon.bug "Report a Unison Bug" Link.reportUnisonBug
                               , ActionMenu.optionItem Icon.bug "Report a Share Bug" Link.reportShareBug
                               , ActionMenu.optionItem Icon.keyboardKey "Keyboard Shortcuts" (Click.onClick ctx.showKeyboardShortcutsModalMsg)
                               , ActionMenu.optionItem Icon.unfoldedMap "Code of Conduct" Link.codeOfConduct
                               , ActionMenu.optionItem Icon.unisonMark "Unison Website" Link.website
                               , ActionMenu.optionItem Icon.github "Unison on GitHub" Link.github
                               ]
                        )
                        |> ActionMenu.fromButton ctx.toggleHelpAndResourcesMenuMsg "Help & Resources"
                        |> ActionMenu.shouldBeOpen (isHelpAndResourcesMenuOpen ctx.openedAppHeaderMenu)
                        |> ActionMenu.withButtonIcon Icon.questionmark
                        |> ActionMenu.withNudge nudge
                        |> ActionMenu.withMaxWidth (Sizing.Rem 15)
                        |> ActionMenu.view
                        |> (\hr -> div [ class "help-and-resources" ] [ hr ])

                loginLink =
                    Link.login ctx.api

                ( navigation, rightSide ) =
                    case ctx.session of
                        Anonymous ->
                            let
                                signIn =
                                    Button.button_ (loginLink ctx.currentUrl) "Sign In"
                                        |> Button.small
                                        |> Button.emphasized
                                        |> Button.view

                                createAccountSheet =
                                    AnchoredOverlay.sheet
                                        (div [ class "create-account-menu" ]
                                            [ Button.iconThenLabel_ (loginLink ctx.currentUrl) Icon.github "Connect with GitHub"
                                                |> Button.medium
                                                |> Button.emphasized
                                                |> Button.view
                                            , div [ class "terms-of-service" ]
                                                [ text "By creating a Unison Account you agree to our "
                                                , Link.view "Terms of Service" Link.termsOfService
                                                , text ". Also see our "
                                                , Link.view "Code of Conduct" Link.codeOfConduct
                                                , text " and our "
                                                , Link.view "Privacy Policy" Link.privacyPolicy
                                                , text "."
                                                ]
                                            ]
                                        )

                                createAccount =
                                    let
                                        createAccountIcon =
                                            if isCreateAccountMenuOpen ctx.openedAppHeaderMenu then
                                                Icon.caretUp

                                            else
                                                Icon.caretDown
                                    in
                                    AnchoredOverlay.anchoredOverlay ctx.toggleCreateAccountMenuMsg
                                        (Button.labelThenIcon ctx.toggleCreateAccountMenuMsg "Create Account" createAccountIcon
                                            |> Button.small
                                            |> Button.positive
                                            |> Button.view
                                        )
                                        |> AnchoredOverlay.withSheet_ (isCreateAccountMenuOpen ctx.openedAppHeaderMenu) createAccountSheet
                                        |> AnchoredOverlay.view

                                nav =
                                    case activeNavItem of
                                        Catalog ->
                                            Navigation.empty |> Navigation.withItems [] navItems.catalog []

                                        _ ->
                                            Navigation.empty |> Navigation.withNoSelectedItems [ navItems.catalog ]
                            in
                            ( nav
                            , [ div [ class "sign-in-nav" ] [ helpAndResources, signIn, createAccount ]
                              , div [ class "sign-in-nav sign-in-nav_mobile" ] [ signIn, createAccount ]
                              ]
                            )

                        SignedIn account ->
                            let
                                nav =
                                    case activeNavItem of
                                        Catalog ->
                                            Navigation.empty |> Navigation.withItems [] navItems.catalog [ navItems.profile account.handle ]

                                        Profile ->
                                            Navigation.empty |> Navigation.withItems [ navItems.catalog ] (navItems.profile account.handle) []

                                        _ ->
                                            Navigation.empty |> Navigation.withNoSelectedItems [ navItems.catalog, navItems.profile account.handle ]

                                avatar =
                                    Avatar.avatar account.avatarUrl (Maybe.map (String.left 1) account.name)
                                        |> Avatar.view

                                viewAccountMenuTrigger isOpen =
                                    let
                                        chevron =
                                            if isOpen then
                                                Icon.chevronUp

                                            else
                                                Icon.chevronDown
                                    in
                                    div [ classList [ ( "account-menu-trigger", True ), ( "account-menu_is-open", isOpen ) ] ]
                                        [ avatar, Icon.view chevron ]

                                newOrgButton =
                                    if account.isSuperAdmin then
                                        Button.iconThenLabel ctx.showNewOrgModal Icon.largePlus "New Org"
                                            |> Button.small
                                            |> Button.positive
                                            |> Button.view

                                    else
                                        UI.nothing

                                accountMenu =
                                    ActionMenu.items
                                        (ActionMenu.optionItem Icon.cog "Account Settings" Link.account)
                                        [ ActionMenu.optionItem Icon.exitDoor "Sign Out" (Link.logout ctx.api ctx.currentUrl) ]
                                        |> ActionMenu.fromCustom ctx.toggleAccountMenuMsg viewAccountMenuTrigger
                                        |> ActionMenu.shouldBeOpen (isAccountMenuOpen ctx.openedAppHeaderMenu)
                                        |> ActionMenu.view
                                        |> (\a -> div [ class "account-menu" ] [ a ])
                            in
                            ( nav, [ newOrgButton, helpAndResources, accountMenu ] )
            in
            UI.AppHeader.appHeader (appTitle (Click.href "/"))
                |> UI.AppHeader.withNavigation navigation
                |> UI.AppHeader.withRightSide rightSide
                |> UI.AppHeader.withViewMode viewMode
                |> UI.AppHeader.view


viewBlank : ViewMode -> Html msg
viewBlank viewMode =
    appTitle Click.Disabled
        |> UI.AppHeader.appHeader
        |> UI.AppHeader.withViewMode viewMode
        |> UI.AppHeader.view
