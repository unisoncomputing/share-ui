module UnisonShare.Page.ProjectPageHeader exposing (..)

import Html exposing (Html, div, label, text)
import Html.Attributes exposing (class)
import UI
import UI.AnchoredOverlay exposing (AnchoredOverlay)
import UI.Button as Button
import UI.Click as Click exposing (Click)
import UI.Icon as Icon
import UI.Navigation as Nav
import UI.PageHeader as PageHeader exposing (PageHeader)
import UI.Tag as Tag
import UI.Tooltip as Tooltip
import UnisonShare.Link as Link
import UnisonShare.Project as Project exposing (Project, ProjectDetails)
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Session as Session exposing (Session)


type ActiveNavItem
    = NoActiveNavItem
    | Overview
    | DocsAndCode
    | Tickets
    | Contributions
    | Releases
    | Settings



-- Create


empty : Project p -> PageHeader msg
empty project =
    let
        context =
            { isActive = False
            , click = Nothing
            , content =
                ProjectListing.projectListing project
                    |> ProjectListing.large
                    |> ProjectListing.verySubdued
                    |> ProjectListing.view
            }
    in
    PageHeader.pageHeader context


loading : ProjectRef -> PageHeader msg
loading projectRef =
    empty { ref = projectRef, visibility = Project.Public }


disabled : ProjectRef -> PageHeader msg
disabled projectRef =
    empty { ref = projectRef, visibility = Project.Public }


error : ProjectRef -> PageHeader msg
error projectRef =
    empty { ref = projectRef, visibility = Project.Public }


type alias NavItems msg =
    { code : Nav.NavItem msg
    , tickets : Nav.NavItem msg
    , contributions : Nav.NavItem msg
    , releases : Nav.NavItem msg
    , settings : Nav.NavItem msg
    }


allNavItems : ProjectDetails -> AnchoredOverlay msg -> NavItems msg
allNavItems project switchBranch =
    let
        defaultBranchRef =
            Project.defaultBrowsingBranch project

        withContributionsCount navItem =
            if project.numActiveContributions > 0 then
                Nav.navItemWithTag (Tag.tag (String.fromInt project.numActiveContributions)) navItem

            else
                navItem

        withTicketsCount navItem =
            if project.numOpenTickets > 0 then
                Nav.navItemWithTag (Tag.tag (String.fromInt project.numOpenTickets)) navItem

            else
                navItem
    in
    { code =
        Nav.navItem "Code" (Link.projectBranchRoot project.ref defaultBranchRef)
            |> Nav.navItemWithIcon Icon.documentCode
            |> Nav.navItemWithAnchoredOverlay switchBranch
    , tickets =
        Nav.navItem "Tickets" (Link.projectTickets project.ref)
            |> Nav.navItemWithIcon Icon.bug
            |> withTicketsCount
    , contributions =
        Nav.navItem "Contributions" (Link.projectContributions project.ref)
            |> Nav.navItemWithIcon Icon.merge
            |> withContributionsCount
    , releases =
        Nav.navItem "Releases" (Link.projectReleases project.ref)
            |> Nav.navItemWithIcon Icon.rocket
    , settings =
        Nav.navItem "Settings" (Link.projectSettings project.ref)
            |> Nav.navItemWithIcon Icon.cog
    }


viewRightSide : Session -> msg -> msg -> msg -> ProjectDetails -> List (Html msg)
viewRightSide session toggleFavMsg toggleSubscriptionMsg useProjectButtonClickMsg project =
    let
        viewKpi icon num description =
            div
                [ class "kpi" ]
                [ Tooltip.view
                    (div
                        [ class "kpi-content" ]
                        [ Icon.view icon
                        , label [ class "kpi-num" ] [ text (String.fromInt num) ]
                        ]
                    )
                    (Tooltip.text description
                        |> Tooltip.tooltip
                        |> Tooltip.withArrow Tooltip.Middle
                        |> Tooltip.withPosition Tooltip.LeftOf
                    )
                ]

        favKpi p icon =
            viewKpi
                icon
                p.numFavs
                "Total number of favorites"

        subscribeButton_ iconOnly icon label =
            if iconOnly then
                Button.icon toggleSubscriptionMsg icon

            else
                Button.iconThenLabel toggleSubscriptionMsg icon label

        subscribeButton iconOnly p =
            case ( session, p.isSubscribed ) of
                ( Session.SignedIn account, Project.NotSubscribed ) ->
                    if account.isSuperAdmin then
                        subscribeButton_ iconOnly Icon.bellSlash "Subscribe"
                            |> Button.outlined
                            |> Button.view

                    else
                        UI.nothing

                ( Session.SignedIn account, Project.Subscribed ) ->
                    if account.isSuperAdmin then
                        subscribeButton_ iconOnly Icon.bell "Unsubscribe"
                            |> Button.outlined
                            |> Button.view

                    else
                        UI.nothing

                ( Session.SignedIn account, Project.JustSubscribed ) ->
                    if account.isSuperAdmin then
                        subscribeButton_ iconOnly Icon.bell "Unsubscribe"
                            |> Button.outlined
                            |> Button.view

                    else
                        UI.nothing

                _ ->
                    UI.nothing

        viewFav p =
            case ( session, p.isFaved ) of
                ( Session.SignedIn _, Project.NotFaved ) ->
                    Click.view [ class "not-faved" ]
                        [ favKpi p Icon.heartOutline ]
                        (Click.onClick toggleFavMsg)

                ( Session.SignedIn _, Project.Faved ) ->
                    Click.view [ class "is-faved" ]
                        [ favKpi p Icon.heart ]
                        (Click.onClick toggleFavMsg)

                ( Session.SignedIn _, Project.JustFaved ) ->
                    Click.view [ class "is-faved just-faved" ]
                        [ favKpi p Icon.heart ]
                        (Click.onClick toggleFavMsg)

                _ ->
                    favKpi p Icon.heartOutline
    in
    [ div [ class "min-lg" ]
        [ div [ class "kpis" ]
            [ viewFav project ]

        {- Turn off release downloads for now
           , viewKpi Icon.download
               (Project.fourWeekTotalDownloads project)
               "Total downloads for the last 4 weeks"
           ]
        -}
        ]
    , div [ class "min-lg" ]
        [ div [ class "actions" ]
            [ subscribeButton False project
            , Button.iconThenLabel useProjectButtonClickMsg Icon.download "Use Project"
                |> Button.positive
                |> Button.view
            ]
        ]
    , div [ class "max-lg" ]
        [ div [ class "actions" ]
            [ subscribeButton True project
            , Button.icon useProjectButtonClickMsg Icon.download
                |> Button.positive
                |> Button.view
            ]
        ]
    ]


type alias ProjectPageHeaderConfig msg =
    { toggleFavMsg : msg
    , toggleSubscriptionMsg : msg
    , useProjectButtonClickMsg : msg
    , mobileNavToggleMsg : msg
    , mobileNavIsOpen : Bool
    , activeNavItem : ActiveNavItem
    , switchBranch : AnchoredOverlay msg
    , contextClick : Click msg
    }


projectPageHeader : Session -> ProjectPageHeaderConfig msg -> ProjectDetails -> PageHeader msg
projectPageHeader session config project =
    let
        context =
            { isActive = config.activeNavItem == Overview
            , click = Just config.contextClick
            , content =
                ProjectListing.projectListing project
                    |> ProjectListing.withClick Link.userProfile Link.projectOverview
                    |> ProjectListing.large
                    |> ProjectListing.view
            }

        allNavItems_ =
            allNavItems project config.switchBranch

        nav =
            case ( Project.canManage project, config.activeNavItem ) of
                ( True, DocsAndCode ) ->
                    Nav.withItems []
                        allNavItems_.code
                        [ allNavItems_.tickets, allNavItems_.contributions, allNavItems_.releases, allNavItems_.settings ]
                        Nav.empty

                ( True, Tickets ) ->
                    Nav.withItems
                        [ allNavItems_.code ]
                        allNavItems_.tickets
                        [ allNavItems_.contributions, allNavItems_.releases, allNavItems_.settings ]
                        Nav.empty

                ( True, Contributions ) ->
                    Nav.withItems
                        [ allNavItems_.code, allNavItems_.tickets ]
                        allNavItems_.contributions
                        [ allNavItems_.releases, allNavItems_.settings ]
                        Nav.empty

                ( True, Releases ) ->
                    Nav.withItems
                        [ allNavItems_.code, allNavItems_.tickets, allNavItems_.contributions ]
                        allNavItems_.releases
                        [ allNavItems_.settings ]
                        Nav.empty

                ( True, Settings ) ->
                    Nav.withItems
                        [ allNavItems_.code, allNavItems_.tickets, allNavItems_.contributions, allNavItems_.releases ]
                        allNavItems_.settings
                        []
                        Nav.empty

                ( True, _ ) ->
                    Nav.withNoSelectedItems
                        [ allNavItems_.code, allNavItems_.tickets, allNavItems_.contributions, allNavItems_.releases, allNavItems_.settings ]
                        Nav.empty

                ( _, DocsAndCode ) ->
                    Nav.withItems
                        []
                        allNavItems_.code
                        [ allNavItems_.tickets, allNavItems_.contributions, allNavItems_.releases ]
                        Nav.empty

                ( _, Tickets ) ->
                    Nav.withItems
                        [ allNavItems_.code ]
                        allNavItems_.tickets
                        [ allNavItems_.contributions, allNavItems_.releases ]
                        Nav.empty

                ( _, Contributions ) ->
                    Nav.withItems
                        [ allNavItems_.code, allNavItems_.tickets ]
                        allNavItems_.contributions
                        [ allNavItems_.releases ]
                        Nav.empty

                ( _, Releases ) ->
                    Nav.withItems
                        [ allNavItems_.code, allNavItems_.tickets, allNavItems_.contributions ]
                        allNavItems_.releases
                        []
                        Nav.empty

                _ ->
                    Nav.withNoSelectedItems
                        [ allNavItems_.code, allNavItems_.tickets, allNavItems_.contributions, allNavItems_.releases ]
                        Nav.empty

        pageHeader_ =
            context
                |> PageHeader.pageHeader
                |> PageHeader.withNavigation
                    { navigation = nav
                    , mobileNavToggleMsg = config.mobileNavToggleMsg
                    , mobileNavIsOpen = config.mobileNavIsOpen
                    }

        pageHeader =
            if config.activeNavItem /= Overview then
                pageHeader_
                    |> PageHeader.withRightSide
                        (viewRightSide
                            session
                            config.toggleFavMsg
                            config.toggleSubscriptionMsg
                            config.useProjectButtonClickMsg
                            project
                        )

            else
                pageHeader_
    in
    pageHeader
