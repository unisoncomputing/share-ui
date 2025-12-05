module UnisonShare.Page.ProjectContributionPageHelpers exposing (..)

import Html exposing (..)
import UI.DateTime as DateTime
import UI.TabList as TabList
import UnisonShare.Contribution as Contribution exposing (ContributionDetails)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus
import UnisonShare.Link as Link
import UnisonShare.Project exposing (ProjectDetails)


type ProjectContributionTab
    = Overview
    | Checks
    | Changes


tabs : ProjectContributionTab -> ProjectDetails -> ContributionDetails -> Html msg
tabs activeTab project contribution =
    let
        includeChanges =
            -- Before this date, we couldn't show diffs on merged
            -- contributions, so we don't want to show the "changes" tab
            DateTime.isAfter Contribution.dateOfHistoricDiffSupport contribution.createdAt || contribution.status == ContributionStatus.InReview

        tab =
            case activeTab of
                Overview ->
                    if includeChanges then
                        TabList.tabList
                            []
                            (TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref))
                            [ TabList.tab "Checks" (Link.projectContributionChecks project.ref contribution.ref)
                            , TabList.tab "Changes" (Link.projectContributionChanges project.ref contribution.ref)
                            ]

                    else
                        TabList.tabList
                            []
                            (TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref))
                            [ TabList.tab "Checks" (Link.projectContributionChecks project.ref contribution.ref)
                            ]

                Checks ->
                    if includeChanges then
                        TabList.tabList
                            [ TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref) ]
                            (TabList.tab "Checks" (Link.projectContributionChecks project.ref contribution.ref))
                            [ TabList.tab "Changes" (Link.projectContributionChanges project.ref contribution.ref)
                            ]

                    else
                        TabList.tabList
                            [ TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref) ]
                            (TabList.tab "Checks" (Link.projectContributionChecks project.ref contribution.ref))
                            []

                Changes ->
                    TabList.tabList
                        [ TabList.tab "Overview" (Link.projectContribution project.ref contribution.ref)
                        , TabList.tab "Checks" (Link.projectContributionChecks project.ref contribution.ref)
                        ]
                        (TabList.tab "Changes" (Link.projectContributionChanges project.ref contribution.ref))
                        []
    in
    TabList.view tab
