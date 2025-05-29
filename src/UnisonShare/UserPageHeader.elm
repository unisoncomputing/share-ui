module UnisonShare.UserPageHeader exposing (..)

import Lib.UserHandle exposing (UserHandle)
import UI.Icon as Icon
import UI.Navigation as Nav
import UI.PageHeader as PageHeader exposing (PageHeader)
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.Link as Link
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.User exposing (User)


type ActiveNavItem
    = UserProfile
    | Contributions
    | Code



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


view : Session -> msg -> Bool -> ActiveNavItem -> UserHandle -> User u -> PageHeader msg
view session toggleMobileNavMsg mobileNavIsOpen activeNavItem handle user =
    let
        context_ =
            ProfileSnippet.profileSnippet user |> ProfileSnippet.view

        context =
            { isActive = activeNavItem == UserProfile
            , click = Just (Link.userProfile handle)
            , content = context_
            }

        nav =
            if Session.isHandle handle session then
                if activeNavItem == Code then
                    Nav.withItems
                        [ Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch ]
                        (Nav.navItem "Non-project Code (deprecated)" (Link.userCode handle) |> Nav.navItemWithIcon Icon.documentCode)
                        []
                        Nav.empty

                else if activeNavItem == Contributions then
                    Nav.withItems
                        []
                        (Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch)
                        [ Nav.navItem "Non-project Code (deprecated)" (Link.userCode handle) |> Nav.navItemWithIcon Icon.documentCode ]
                        Nav.empty

                else
                    Nav.withNoSelectedItems
                        [ Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch
                        , Nav.navItem "Non-project Code (deprecated)" (Link.userCode handle)
                            |> Nav.navItemWithIcon Icon.documentCode
                        ]
                        Nav.empty

            else if activeNavItem == Contributions then
                Nav.withItems
                    []
                    (Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch)
                    []
                    Nav.empty

            else
                Nav.withNoSelectedItems
                    [ Nav.navItem "Contributions" (Link.userContributions handle) |> Nav.navItemWithIcon Icon.branch
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
