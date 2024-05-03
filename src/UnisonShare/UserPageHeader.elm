module UnisonShare.UserPageHeader exposing (..)

import Lib.UserHandle exposing (UserHandle)
import UI.Icon as Icon
import UI.Navigation as Nav
import UI.PageHeader as PageHeader exposing (PageHeader)
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.Link as Link
import UnisonShare.User exposing (User)


type ActiveNavItem
    = UserProfile
    | Code
    | Contributions



-- CREATE


empty : PageHeader msg
empty =
    let
        context =
            { isActive = False
            , click = Nothing
            , content = ProfileSnippet.loading |> ProfileSnippet.view
            }
    in
    PageHeader.pageHeader context


loading : PageHeader msg
loading =
    empty


error : PageHeader msg
error =
    empty


view : msg -> Bool -> ActiveNavItem -> UserHandle -> User u -> PageHeader msg
view toggleMobileNavMsg mobileNavIsOpen activeNavItem handle user =
    let
        context_ =
            ProfileSnippet.profileSnippet user |> ProfileSnippet.view

        context =
            { isActive = activeNavItem == UserProfile
            , click = Just (Link.userProfile handle)
            , content = context_
            }

        nav =
            if activeNavItem == Code then
                Nav.withItems
                    []
                    (Nav.navItem "Code" (Link.userCodeRoot handle) |> Nav.navItemWithIcon Icon.ability)
                    [ Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch ]
                    Nav.empty

            else if activeNavItem == Contributions then
                Nav.withItems
                    [ Nav.navItem "Code" (Link.userCodeRoot handle) |> Nav.navItemWithIcon Icon.ability ]
                    (Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch)
                    []
                    Nav.empty

            else
                Nav.withNoSelectedItems
                    [ Nav.navItem "Code" (Link.userCodeRoot handle)
                        |> Nav.navItemWithIcon Icon.ability
                    , Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch
                    ]
                    Nav.empty
    in
    context
        |> PageHeader.pageHeader
        |> PageHeader.withNavigation
            { navigation = nav
            , mobileNavToggleMsg = toggleMobileNavMsg
            , mobileNavIsOpen = mobileNavIsOpen
            }
