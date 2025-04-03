module UnisonShare.OrgPageHeader exposing (..)

import Lib.UserHandle exposing (UserHandle)
import UI.PageHeader as PageHeader exposing (PageHeader)
import UI.ProfileSnippet as ProfileSnippet
import UnisonShare.Link as Link
import UnisonShare.Org exposing (Org)


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


error : PageHeader msg
error =
    empty


view : msg -> Bool -> ActiveNavItem -> UserHandle -> Org o -> PageHeader msg
view _ _ activeNavItem handle org =
    let
        context_ =
            ProfileSnippet.profileSnippet org |> ProfileSnippet.view

        context =
            { isActive = activeNavItem == OrgProfile
            , click = Just (Link.userProfile handle)
            , content = context_
            }
    in
    context
        |> PageHeader.pageHeader
