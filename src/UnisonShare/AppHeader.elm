module UnisonShare.AppHeader exposing (..)

import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, classList)
import Lib.HttpApi exposing (HttpApi)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Time
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
import UnisonShare.Account as Account
import UnisonShare.Link as Link
import UnisonShare.Session exposing (Session(..))
import Url exposing (Url)
import WhatsNew exposing (WhatsNew)


type AppHeader
    = Disabled
    | AppHeader



-- CREATE


empty : AppHeader
empty =
    Disabled


appHeader : AppHeader
appHeader =
    AppHeader



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
        Disabled ->
            viewBlank

        AppHeader ->
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

                helpAndResourcesItems =
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

                helpAndResources isMobile =
                    let
                        button =
                            if isMobile then
                                ActionMenu.fromIconButton ctx.toggleHelpAndResourcesMenuMsg Icon.questionmark helpAndResourcesItems

                            else
                                ActionMenu.fromButton ctx.toggleHelpAndResourcesMenuMsg "Help & Resources" helpAndResourcesItems
                    in
                    button
                        |> ActionMenu.withButtonIcon Icon.questionmark
                        |> ActionMenu.shouldBeOpen (isHelpAndResourcesMenuOpen ctx.openedAppHeaderMenu)
                        |> ActionMenu.withButtonIcon Icon.questionmark
                        |> ActionMenu.withNudge nudge
                        |> ActionMenu.withMaxWidth (Sizing.Rem 15)
                        |> ActionMenu.view
                        |> (\hr -> div [ class "help-and-resources" ] [ hr ])

                loginLink =
                    Link.login ctx.api

                rightSide =
                    case ctx.session of
                        Anonymous ->
                            let
                                signIn =
                                    Button.button_ (loginLink ctx.currentUrl) "Sign In"
                                        |> Button.small
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
                            in
                            [ div
                                [ class "sign-in-nav sign-in-nav_desktop" ]
                                [ helpAndResources False
                                , signIn
                                , createAccount
                                ]
                            , div [ class "sign-in-nav sign-in-nav_mobile" ]
                                [ helpAndResources True
                                , signIn
                                , createAccount
                                ]
                            ]

                        SignedIn account ->
                            let
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
                                    Button.iconThenLabel ctx.showNewOrgModal Icon.largePlus "New Org"
                                        |> Button.small
                                        |> Button.positive
                                        |> Button.view

                                notifications =
                                    if account.hasUnreadNotifications then
                                        div [ class "notifications" ]
                                            [ Button.icon_ Link.notifications Icon.bellRing
                                                |> Button.small
                                                |> Button.view
                                            , Nudge.nudge
                                                |> Nudge.view
                                            ]

                                    else
                                        Button.icon_ Link.notifications Icon.bell
                                            |> Button.small
                                            |> Button.view

                                orgs_ =
                                    account.organizationMemberships
                                        |> List.map (\(Account.OrganizationMembership h) -> h)
                                        |> List.map (\h -> ActionMenu.optionItem Icon.factory (UserHandle.toString h) (Link.orgProfile h))

                                orgs =
                                    if List.isEmpty orgs_ then
                                        []

                                    else
                                        ActionMenu.dividerItem :: orgs_ ++ [ ActionMenu.dividerItem ]

                                accountMenu =
                                    ActionMenu.items
                                        (ActionMenu.optionItem Icon.user (UserHandle.toString account.handle) (Link.userProfile account.handle))
                                        (orgs
                                            ++ [ ActionMenu.optionItem Icon.cog "Account Settings" Link.account
                                               , ActionMenu.optionItem Icon.exitDoor "Sign Out" (Link.logout ctx.api ctx.currentUrl)
                                               ]
                                        )
                                        |> ActionMenu.fromCustom ctx.toggleAccountMenuMsg viewAccountMenuTrigger
                                        |> ActionMenu.shouldBeOpen (isAccountMenuOpen ctx.openedAppHeaderMenu)
                                        |> ActionMenu.view
                                        |> (\a -> div [ class "account-menu" ] [ a ])
                            in
                            [ div [ class "signed-in-nav signed-in-nav_desktop" ]
                                [ newOrgButton
                                , notifications
                                , helpAndResources False
                                , accountMenu
                                ]
                            , div [ class "signed-in-nav signed-in-nav_mobile" ]
                                [ newOrgButton
                                , notifications
                                , helpAndResources True
                                , accountMenu
                                ]
                            ]
            in
            UI.AppHeader.appHeader (appTitle (Click.href "/"))
                |> UI.AppHeader.withRightSide rightSide
                |> UI.AppHeader.view


viewBlank : Html msg
viewBlank =
    appTitle Click.Disabled
        |> UI.AppHeader.appHeader
        |> UI.AppHeader.view
