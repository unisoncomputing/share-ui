module UnisonShare.Link exposing (..)

import Code.BranchRef exposing (BranchRef)
import Code.Definition.Reference exposing (Reference)
import Code.FullyQualifiedName exposing (FQN)
import Code.Perspective as Perspective exposing (Perspective)
import Code.Version exposing (Version)
import Html exposing (Html, text)
import Lib.HttpApi as HttpApi exposing (HttpApi)
import Lib.UserHandle exposing (UserHandle)
import UI.Click as Click exposing (Click)
import UnisonShare.BranchDiff.ChangeLineId exposing (ChangeLineId)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route exposing (Route)
import UnisonShare.Ticket.TicketRef exposing (TicketRef)
import Url exposing (Url)



{-

   Link
   ====

   Various UI.Click link helpers for Routes and external links

-}
-- EXTERNAL


link : String -> Click msg
link url =
    Click.externalHref url


unisonCloudWebsite : Click msg
unisonCloudWebsite =
    Click.externalHref "https://unison.cloud"


unisonCloudWebsiteByoc : Click msg
unisonCloudWebsiteByoc =
    Click.externalHref "https://unison.cloud/byoc"


unisonCloudWebsiteLearn : Click msg
unisonCloudWebsiteLearn =
    Click.externalHref "https://unison.cloud/learn"


website : Click msg
website =
    Click.externalHref "https://unison-lang.org"


conference : Click msg
conference =
    Click.externalHref "https://unison-lang.org/conference"


aoc2024 : Click msg
aoc2024 =
    Click.externalHref "https://unison-lang.org/adventofcode2024"


whatsNew : Click msg
whatsNew =
    Click.externalHref "https://unison-lang.org/whats-new"


whatsNewPost : String -> Click msg
whatsNewPost postPath =
    Click.externalHref ("https://unison-lang.org/whats-new/" ++ postPath)


github : Click msg
github =
    Click.externalHref "https://github.com/unisonweb/unison"


githubReleases : Click msg
githubReleases =
    Click.externalHref "https://github.com/unisonweb/unison/releases"


githubRelease : String -> Click msg
githubRelease releaseTag =
    Click.externalHref ("https://github.com/unisonweb/unison/releases/tag/" ++ releaseTag)


reportUnisonBug : Click msg
reportUnisonBug =
    Click.externalHref "https://github.com/unisonweb/unison/issues/new"


reportShareBug : Click msg
reportShareBug =
    Click.externalHref "https://github.com/unisoncomputing/share-ui/issues/new"


docs : Click msg
docs =
    Click.externalHref "https://unison-lang.org/docs"


unisonShareDocs : Click msg
unisonShareDocs =
    Click.externalHref "https://unison-lang.org/learn/tooling/unison-share"


tour : Click msg
tour =
    Click.externalHref "https://unison-lang.org/docs/tour"


codeOfConduct : Click msg
codeOfConduct =
    Click.externalHref "https://www.unison-lang.org/community/code-of-conduct/"


status : Click msg
status =
    Click.externalHref "https://unison.statuspage.io"


discord : Click msg
discord =
    Click.externalHref "https://unison-lang.com/discord"


login : HttpApi -> Url -> Click msg
login api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode
    in
    Click.externalHref_ Click.Self (HttpApi.baseApiUrl api ++ "login?return_to=" ++ returnTo_)


logout : HttpApi -> Url -> Click msg
logout api returnTo =
    let
        returnTo_ =
            returnTo
                |> Url.toString
                |> Url.percentEncode
    in
    Click.externalHref_ Click.Self (HttpApi.baseApiUrl api ++ "logout?return_to=" ++ returnTo_)



-- WITHIN SHARE


home : Click msg
home =
    toClick Route.catalog


catalog : Click msg
catalog =
    toClick Route.catalog


account : Click msg
account =
    toClick Route.account


userProfile : UserHandle -> Click msg
userProfile =
    Route.userProfile >> toClick


userDefinition : UserHandle -> Perspective -> Reference -> Click msg
userDefinition handle_ pers ref =
    toClick (Route.userDefinition handle_ pers ref)


userCodeRoot : UserHandle -> Click msg
userCodeRoot handle_ =
    let
        pers =
            Perspective.relativeRootPerspective
    in
    toClick (Route.userCodeRoot handle_ pers)


userNamespaceRoot : UserHandle -> FQN -> Click msg
userNamespaceRoot handle_ namespaceFqn =
    let
        pers =
            Perspective.namespacePerspective namespaceFqn
    in
    toClick (Route.userCodeRoot handle_ pers)


userContributions : UserHandle -> Click msg
userContributions =
    Route.userContributions >> toClick


projectOverview : ProjectRef -> Click msg
projectOverview projectRef_ =
    toClick (Route.projectOverview projectRef_)


projectBranches : ProjectRef -> Click msg
projectBranches projectRef_ =
    toClick (Route.projectBranches projectRef_)


projectBranchDefinition_ : ProjectRef -> BranchRef -> Perspective -> Reference -> Click msg
projectBranchDefinition_ projectRef_ branchRef pers ref =
    toClick (Route.projectBranchDefinition projectRef_ branchRef pers ref)


projectBranchDefinition : ProjectRef -> BranchRef -> Reference -> Click msg
projectBranchDefinition projectRef_ branchRef ref =
    projectBranchDefinition_ projectRef_ branchRef Perspective.relativeRootPerspective ref


projectBranchRoot : ProjectRef -> BranchRef -> Click msg
projectBranchRoot projectRef_ branchRef =
    let
        pers =
            Perspective.relativeRootPerspective
    in
    toClick (Route.projectBranchRoot projectRef_ branchRef pers)


projectNamespaceRoot : ProjectRef -> BranchRef -> FQN -> Click msg
projectNamespaceRoot projectRef_ branchRef namespaceFqn =
    let
        pers =
            Perspective.namespacePerspective namespaceFqn
    in
    toClick (Route.projectBranchRoot projectRef_ branchRef pers)


projectRelease : ProjectRef -> Version -> Click msg
projectRelease projectRef_ version =
    toClick (Route.projectRelease projectRef_ version)


projectReleases : ProjectRef -> Click msg
projectReleases projectRef_ =
    toClick (Route.projectReleases projectRef_)


projectContribution : ProjectRef -> ContributionRef -> Click msg
projectContribution projectRef_ contribRef =
    toClick (Route.projectContribution projectRef_ contribRef)


projectContributionOverview : ProjectRef -> ContributionRef -> Click msg
projectContributionOverview projectRef_ contribRef =
    toClick (Route.projectContribution projectRef_ contribRef)


projectContributionChanges : ProjectRef -> ContributionRef -> Click msg
projectContributionChanges projectRef_ contribRef =
    toClick (Route.projectContributionChanges projectRef_ contribRef)


projectContributionChange : ProjectRef -> ContributionRef -> ChangeLineId -> Click msg
projectContributionChange projectRef_ contribRef changeLineId =
    toClick (Route.projectContributionChange projectRef_ contribRef changeLineId)


projectContributions : ProjectRef -> Click msg
projectContributions projectRef_ =
    toClick (Route.projectContributions projectRef_)


projectTicket : ProjectRef -> TicketRef -> Click msg
projectTicket projectRef_ ticketRef =
    toClick (Route.projectTicket projectRef_ ticketRef)


projectTickets : ProjectRef -> Click msg
projectTickets projectRef_ =
    toClick (Route.projectTickets projectRef_)


projectSettings : ProjectRef -> Click msg
projectSettings projectRef_ =
    toClick (Route.projectSettings projectRef_)


termsOfService : Click msg
termsOfService =
    toClick Route.termsOfService


privacyPolicy : Click msg
privacyPolicy =
    toClick Route.privacyPolicy


ucmConnected : Click msg
ucmConnected =
    toClick Route.ucmConnected


cloud : Click msg
cloud =
    toClick Route.cloud



-- VIEW


view : String -> Click msg -> Html msg
view label click =
    view_ (text label) click


view_ : Html msg -> Click msg -> Html msg
view_ label_ click =
    Click.view [] [ label_ ] click



-- HELPERS


toClick : Route -> Click msg
toClick =
    Route.toUrlString >> Click.href
