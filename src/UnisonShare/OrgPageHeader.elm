module UnisonShare.OrgPageHeader exposing (..)

import Lib.UserHandle exposing (UserHandle)
import UI.Icon as Icon
import UI.Navigation as Nav exposing (Navigation)
import UI.PageHeader as PageHeader exposing (PageHeader)
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.Link as Link
import UnisonShare.Org as Org exposing (OrgWithPermissions)


type ActiveNavItem
    = OrgProfile
    | People
    | Settings



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


error : UserHandle -> PageHeader msg
error handle =
    let
        context_ =
            ProfileSnippet.profileSnippet
                { handle = handle, avatarUrl = Nothing, name = Nothing }
                |> ProfileSnippet.view

        context =
            { isActive = False
            , click = Just (Link.orgProfile handle)
            , content = context_
            }
    in
    PageHeader.pageHeader context


nav : ActiveNavItem -> OrgWithPermissions o -> Navigation msg
nav activeNavItem org =
    let
        allNavItems =
            { people =
                Nav.navItem "People" (Link.orgPeople org.handle)
                    |> Nav.navItemWithIcon Icon.userGroup
            , settings =
                Nav.navItem "Settings" (Link.orgSettings org.handle)
                    |> Nav.navItemWithIcon Icon.cog
            }
    in
    case activeNavItem of
        OrgProfile ->
            Nav.withNoSelectedItems [ allNavItems.people ] Nav.empty

        People ->
            Nav.withItems [] allNavItems.people [] Nav.empty

        Settings ->
            Nav.withNoSelectedItems [ allNavItems.people ] Nav.empty


view : msg -> Bool -> ActiveNavItem -> UserHandle -> OrgWithPermissions o -> PageHeader msg
view mobileNavToggleMsg mobileNavIsOpen activeNavItem handle org =
    let
        context_ =
            ProfileSnippet.profileSnippet org |> ProfileSnippet.view

        context =
            { isActive = activeNavItem == OrgProfile
            , click = Just (Link.orgProfile handle)
            , content = context_
            }

        pageHeader =
            PageHeader.pageHeader context
    in
    if Org.canManage org then
        PageHeader.withNavigation
            { navigation = nav activeNavItem org
            , mobileNavToggleMsg = mobileNavToggleMsg
            , mobileNavIsOpen = mobileNavIsOpen
            }
            pageHeader

    else
        pageHeader
