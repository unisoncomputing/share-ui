module UnisonShare.Route exposing
    ( CodeRoute(..)
    , ProjectContributionRoute(..)
    , ProjectRoute(..)
    , Route(..)
    , UserRoute(..)
    , acceptTerms
    , account
    , catalog
    , cloud
    , codeRoot
    , definition
    , fromUrl
    , navigate
    , privacyPolicy
    , projectBranch
    , projectBranchDefinition
    , projectBranchRoot
    , projectBranches
    , projectContribution
    , projectContributionChange
    , projectContributionChanges
    , projectContributions
    , projectOverview
    , projectRelease
    , projectReleases
    , projectSettings
    , projectTicket
    , projectTickets
    , replacePerspective
    , termsOfService
    , toRoute
    , toUrl
    , toUrlPattern
    , toUrlString
    , ucmConnected
    , userCode
    , userCodeRoot
    , userContributions
    , userDefinition
    , userProfile
    )

import Browser.Navigation as Nav
import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.Definition.Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN
import Code.Hash as Hash
import Code.HashQualified exposing (HashQualified(..))
import Code.Perspective as Perspective exposing (Perspective, PerspectiveParams)
import Code.UrlParsers
    exposing
        ( b
        , branchRef
        , end
        , perspectiveParams
        , projectSlug
        , reference
        , s
        , slash
        , userHandle
        , version
        )
import Code.Version as Version exposing (Version)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import List.Nonempty as NEL
import Parser exposing ((|.), (|=), Parser, oneOf, succeed, symbol)
import UI.ViewMode as ViewMode exposing (ViewMode)
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppError as AppError exposing (AppError)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId exposing (ChangeLineId)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import Url exposing (Url)
import Url.Builder exposing (relative, string)


type CodeRoute
    = CodeRoot PerspectiveParams
    | Definition PerspectiveParams Reference


type UserRoute
    = UserProfile
    | UserContributions
    | UserCode CodeRoute


type ProjectContributionRoute
    = ProjectContributionOverview
    | ProjectContributionChanges (Maybe ChangeLineId)


type ProjectRoute
    = ProjectOverview
    | ProjectBranches
    | ProjectBranch BranchRef ViewMode CodeRoute
    | ProjectRelease Version
    | ProjectReleases
    | ProjectTicket TicketRef
    | ProjectTickets
    | ProjectContribution ContributionRef ProjectContributionRoute
    | ProjectContributions
    | ProjectSettings


type Route
    = Catalog
    | Account
    | User UserHandle UserRoute
    | Project ProjectRef ProjectRoute
    | TermsOfService
    | AcceptTerms (Maybe Url)
    | PrivacyPolicy
    | UcmConnected
    | Cloud
    | NotFound String
    | Error AppError



-- CREATE ---------------------------------------------------------------------


catalog : Route
catalog =
    Catalog


account : Route
account =
    Account


cloud : Route
cloud =
    Cloud


userProfile : UserHandle -> Route
userProfile handle_ =
    User handle_ UserProfile


userCode : UserHandle -> CodeRoute -> Route
userCode handle_ codeRoute =
    User handle_ (UserCode codeRoute)


userDefinition : UserHandle -> Perspective -> Reference -> Route
userDefinition handle_ pers ref =
    let
        pp =
            Perspective.toParams pers
    in
    User handle_ (UserCode (Definition pp ref))


userCodeRoot : UserHandle -> Perspective -> Route
userCodeRoot handle_ pers =
    let
        pp =
            Perspective.toParams pers
    in
    User handle_ (UserCode (CodeRoot pp))


userContributions : UserHandle -> Route
userContributions handle_ =
    User handle_ UserContributions


projectOverview : ProjectRef -> Route
projectOverview projectRef_ =
    Project projectRef_ ProjectOverview


projectBranch : ProjectRef -> BranchRef -> CodeRoute -> Route
projectBranch projectRef_ branchRef_ codeRoute =
    Project projectRef_ (ProjectBranch branchRef_ ViewMode.Regular codeRoute)


projectBranches : ProjectRef -> Route
projectBranches projectRef_ =
    Project projectRef_ ProjectBranches


projectRelease : ProjectRef -> Version -> Route
projectRelease projectRef_ version =
    Project projectRef_ (ProjectRelease version)


projectReleases : ProjectRef -> Route
projectReleases projectRef_ =
    Project projectRef_ ProjectReleases


projectContribution : ProjectRef -> ContributionRef -> Route
projectContribution projectRef_ contribRef =
    Project projectRef_ (ProjectContribution contribRef ProjectContributionOverview)


projectContributionChanges : ProjectRef -> ContributionRef -> Route
projectContributionChanges projectRef_ contribRef =
    Project projectRef_ (ProjectContribution contribRef (ProjectContributionChanges Nothing))


projectContributionChange : ProjectRef -> ContributionRef -> ChangeLineId -> Route
projectContributionChange projectRef_ contribRef changeLineId =
    Project projectRef_ (ProjectContribution contribRef (ProjectContributionChanges (Just changeLineId)))


projectContributions : ProjectRef -> Route
projectContributions projectRef_ =
    Project projectRef_ ProjectContributions


projectTicket : ProjectRef -> TicketRef -> Route
projectTicket projectRef_ ticketRef =
    Project projectRef_ (ProjectTicket ticketRef)


projectTickets : ProjectRef -> Route
projectTickets projectRef_ =
    Project projectRef_ ProjectTickets


projectSettings : ProjectRef -> Route
projectSettings projectRef_ =
    Project projectRef_ ProjectSettings


projectBranchDefinition : ProjectRef -> BranchRef -> Perspective -> Reference -> Route
projectBranchDefinition projectRef_ branchRef_ pers ref =
    let
        pp =
            Perspective.toParams pers
    in
    Project projectRef_ (ProjectBranch branchRef_ ViewMode.Regular (Definition pp ref))


projectBranchRoot : ProjectRef -> BranchRef -> Perspective -> Route
projectBranchRoot projectRef_ branchRef pers =
    let
        pp =
            Perspective.toParams pers
    in
    Project projectRef_ (ProjectBranch branchRef ViewMode.Regular (CodeRoot pp))


definition : Perspective -> Reference -> CodeRoute
definition pers ref =
    Definition (Perspective.toParams pers) ref


codeRoot : Perspective -> CodeRoute
codeRoot pers =
    CodeRoot (Perspective.toParams pers)


replacePerspective : Maybe Reference -> Perspective -> CodeRoute
replacePerspective ref pers =
    let
        pp =
            Perspective.toParams pers
    in
    case ref of
        Just r ->
            Definition pp r

        Nothing ->
            CodeRoot pp


termsOfService : Route
termsOfService =
    TermsOfService


acceptTerms : Maybe Url -> Route
acceptTerms continueUrl =
    AcceptTerms continueUrl


privacyPolicy : Route
privacyPolicy =
    PrivacyPolicy


ucmConnected : Route
ucmConnected =
    UcmConnected



-- PARSE ----------------------------------------------------------------------


toRoute : Maybe String -> Parser Route
toRoute queryString =
    oneOf
        [ b homeParser
        , b catalogParser
        , b accountParser
        , b userParser
        , b (projectParser queryString) -- Specifically comes _after_ userParser because project slugs share the url space with user pages
        , b termsOfServiceParser
        , b (acceptTermsParser queryString)
        , b privacyPolicyParser
        , b ucmConnectedParser
        , b cloudParser
        , b (errorParser queryString)
        ]


homeParser : Parser Route
homeParser =
    succeed Catalog |. end


catalogParser : Parser Route
catalogParser =
    succeed Catalog |. slash |. s "catalog"


accountParser : Parser Route
accountParser =
    succeed Account |. slash |. s "account"


termsOfServiceParser : Parser Route
termsOfServiceParser =
    succeed TermsOfService |. slash |. s "terms-of-service"


acceptTermsParser : Maybe String -> Parser Route
acceptTermsParser queryString =
    let
        urlParser : Parser Url
        urlParser =
            let
                parseMaybe url =
                    case url of
                        Just s_ ->
                            Parser.succeed s_

                        Nothing ->
                            Parser.problem "Invalid Url"
            in
            Parser.chompUntilEndOr "&"
                |> Parser.getChompedString
                |> Parser.map Url.fromString
                |> Parser.andThen parseMaybe

        continueUrlParser : Parser Url
        continueUrlParser =
            succeed identity |. s "continue" |. symbol "=" |= urlParser |. end

        continueUrl : Maybe Url
        continueUrl =
            queryString
                |> Maybe.withDefault ""
                |> Parser.run continueUrlParser
                |> Result.toMaybe
    in
    succeed (AcceptTerms continueUrl) |. slash |. s "accept-terms"


privacyPolicyParser : Parser Route
privacyPolicyParser =
    succeed PrivacyPolicy |. slash |. s "privacy-policy"


ucmConnectedParser : Parser Route
ucmConnectedParser =
    succeed UcmConnected |. slash |. s "ucm-connected"


cloudParser : Parser Route
cloudParser =
    succeed Cloud |. slash |. s "cloud"


errorParser : Maybe String -> Parser Route
errorParser queryString =
    let
        appErrorQueryParamParser : Parser AppError
        appErrorQueryParamParser =
            oneOf
                [ b (succeed AppError.AccountCreationGitHubPermissionsRejected |. s "appError=AccountCreationGitHubPermissionsRejected")
                , b (succeed AppError.AccountCreationHandleAlreadyTaken |. s "appError=AccountCreationHandleAlreadyTaken")
                , b (succeed AppError.UnspecifiedError |. s "appError=UnspecifiedError")
                , b (succeed AppError.UnspecifiedError)
                ]

        appError : AppError
        appError =
            queryString
                |> Maybe.withDefault ""
                |> Parser.run appErrorQueryParamParser
                |> Result.withDefault AppError.UnspecifiedError
    in
    succeed (Error appError) |. slash |. s "error"


viewModeQueryParamParser : Parser ViewMode
viewModeQueryParamParser =
    oneOf
        [ b (succeed ViewMode.Regular |. s "viewMode=regular")
        , b (succeed ViewMode.Presentation |. s "viewMode=presentation")
        , b (succeed ViewMode.Regular)
        ]


userParser : Parser Route
userParser =
    let
        userCode_ h c =
            User h (UserCode c)

        userContrib h =
            User h UserContributions

        userProfile_ h =
            User h UserProfile
    in
    oneOf
        [ b (succeed userProfile_ |. slash |= userHandle |. end)
        , b (succeed userContrib |. slash |= userHandle |. slash |. s "p" |. slash |. s "contributions" |. end)
        , b (succeed userCode_ |. slash |= userHandle |. slash |. s "code" |. slash |= codeParser)
        , b (succeed userCode_ |. slash |= userHandle |. slash |. s "p" |. slash |. s "code" |. slash |= codeParser)
        ]


projectParser : Maybe String -> Parser Route
projectParser queryString =
    let
        viewMode =
            queryString
                |> Maybe.withDefault ""
                |> Parser.run viewModeQueryParamParser
                |> Result.withDefault ViewMode.Regular

        projectOverview_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectOverview

        projectBranches_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectBranches

        projectBranch_ handle slug branchRef c =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectBranch branchRef viewMode c)

        projectBranchRoot_ handle slug branchRef =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectBranch branchRef viewMode (CodeRoot (Perspective.ByRoot Perspective.Relative)))

        projectRelease_ handle slug version_ =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectRelease version_)

        projectReleases_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectReleases

        projectSettings_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectSettings

        projectContribution_ handle slug ref =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectContribution ref ProjectContributionOverview)

        projectContributionChange_ handle slug ref fqn =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectContribution ref (ProjectContributionChanges (Just fqn)))

        projectContributionChanges_ handle slug ref =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectContribution ref (ProjectContributionChanges Nothing))

        projectContributions_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectContributions

        projectTicket_ handle slug ref =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectTicket ref)

        projectTickets_ handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps ProjectTickets
    in
    oneOf
        [ b (succeed projectOverview_ |. slash |= userHandle |. slash |= projectSlug |. end)
        , b (succeed projectBranches_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "branches" |. end)
        , b (succeed projectBranch_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "code" |. slash |= branchRef |. slash |= codeParser)
        , b (succeed projectBranchRoot_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "code" |. slash |= branchRef |. end)
        , b (succeed projectRelease_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "releases" |. slash |= version |. end)
        , b (succeed projectReleases_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "releases" |. end)
        , b (succeed projectContributionChange_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. slash |= ContributionRef.fromUrl |. slash |. s "changes" |. slash |= ChangeLineId.fromUrl |. end)
        , b (succeed projectContributionChanges_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. slash |= ContributionRef.fromUrl |. slash |. s "changes" |. end)
        , b (succeed projectContribution_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. slash |= ContributionRef.fromUrl |. end)
        , b (succeed projectContributions_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. end)
        , b (succeed projectTicket_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "tickets" |. slash |= TicketRef.fromUrl |. end)
        , b (succeed projectTickets_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "tickets" |. end)
        , b (succeed projectSettings_ |. slash |= userHandle |. slash |= projectSlug |. slash |. s "settings" |. end)
        ]


codeParser : Parser CodeRoute
codeParser =
    oneOf [ b codeDefinitionParser, b codeRootParser ]


codeRootParser : Parser CodeRoute
codeRootParser =
    succeed CodeRoot |= perspectiveParams


codeDefinitionParser : Parser CodeRoute
codeDefinitionParser =
    succeed Definition |= perspectiveParams |. slash |= reference


{-| In environments like Unison Local, the UI is served with a base path

This means that a route to a definition might look like:

  - "/:some-token/ui/latest/terms/base/List/map"
    (where "/:some-token/ui/" is the base path.)

The base path is determined outside of the Elm app using the <base> tag in the
<head> section of the document. The Browser uses this tag to prefix all links.

The base path must end in a slash for links to work correctly, but our parser
expects a path to starts with a slash. When parsing the URL we thus pre-process
the path to strip the base path and ensure a slash prefix before we parse.

---

Note that this doesn't use Url.Parser to parse the URL as you'd normally see in
Elm apps. This is because of how we represent Fully Qualified Names in the url
by turning `.` into `/`:

`base.data.List.map` is represented in the url as `base/data/List/map`

-}
fromUrl : String -> Url -> Route
fromUrl basePath url =
    let
        stripBasePath path =
            if basePath == "/" then
                path

            else
                String.replace basePath "" path

        ensureSlashPrefix path =
            if String.startsWith "/" path then
                path

            else
                "/" ++ path

        parse queryString path =
            path
                |> Parser.run (toRoute queryString)
                |> Result.withDefault (NotFound path)
    in
    url
        |> .path
        |> stripBasePath
        |> ensureSlashPrefix
        |> parse url.query



-- HELPERS --------------------------------------------------------------------


{-| Creates the string of a route in a de-parameritized way for deduping pages in metrics events
-}
toUrlPattern : Route -> String
toUrlPattern r =
    let
        codePattern codeRoute =
            case codeRoute of
                CodeRoot (Perspective.ByRoot _) ->
                    ""

                CodeRoot (Perspective.ByNamespace _ _) ->
                    "/latest/namespaces/:fqn"

                Definition (Perspective.ByRoot _) ref ->
                    "/latest/" ++ refPattern ref

                Definition (Perspective.ByNamespace _ _) ref ->
                    "/latest/namespaces/:fqn/;/" ++ refPattern ref

        refPattern ref =
            case ref of
                TypeReference _ ->
                    "types/:fqn"

                TermReference _ ->
                    "terms/:fqn"

                AbilityConstructorReference _ ->
                    "ability-constructors/:fqn"

                DataConstructorReference _ ->
                    "data-constructors/:fqn"
    in
    case r of
        Catalog ->
            "catalog"

        Account ->
            "account"

        User _ UserProfile ->
            ":handle"

        User _ (UserCode codeRoute) ->
            ":handle/p/code" ++ codePattern codeRoute

        User _ UserContributions ->
            ":handle/p/contributions"

        Project _ ProjectOverview ->
            ":handle/:project-slug"

        Project _ ProjectBranches ->
            ":handle/:project-slug/branches"

        Project _ (ProjectBranch _ _ codeRoute) ->
            ":handle/:project-slug/code/:branch-ref/" ++ codePattern codeRoute

        Project _ (ProjectRelease _) ->
            ":handle/:project-slug/releases/:version"

        Project _ ProjectReleases ->
            ":handle/:project-slug/releases"

        Project _ (ProjectContribution _ ProjectContributionOverview) ->
            ":handle/:project-slug/contributions/:contribution-ref"

        Project _ (ProjectContribution _ (ProjectContributionChanges Nothing)) ->
            ":handle/:project-slug/contributions/:contribution-ref/changes"

        Project _ (ProjectContribution _ (ProjectContributionChanges (Just _))) ->
            ":handle/:project-slug/contributions/:contribution-ref/changes/:definition-name"

        Project _ ProjectContributions ->
            ":handle/:project-slug/contributions"

        Project _ (ProjectTicket _) ->
            ":handle/:project-slug/tickets/:ticket-ref"

        Project _ ProjectTickets ->
            ":handle/:project-slug/tickets"

        Project _ ProjectSettings ->
            ":handle/:project-slug/settings"

        TermsOfService ->
            "terms-of-service"

        AcceptTerms _ ->
            "accept-terms"

        PrivacyPolicy ->
            "privacy-policy"

        UcmConnected ->
            "ucm-connected"

        Cloud ->
            "cloud"

        Error _ ->
            "error"

        NotFound _ ->
            "404"


toUrl : AppContext -> Route -> Url
toUrl appContext route =
    let
        currentUrl =
            appContext.currentUrl
    in
    { currentUrl | path = "/" ++ toUrlString route }


toUrlString : Route -> String
toUrlString route =
    let
        namespaceSuffix =
            ";"

        hqToPath hq =
            case hq of
                NameOnly fqn ->
                    fqn |> FQN.toUrlSegments |> NEL.toList

                HashOnly h ->
                    [ Hash.toUrlString h ]

                HashQualified _ h ->
                    -- TODO: Currently not supported, since we favor the hash
                    -- because HashQualified url parsing is broken
                    [ Hash.toUrlString h ]

        refPath ref =
            case ref of
                TypeReference hq ->
                    "types" :: hqToPath hq

                TermReference hq ->
                    "terms" :: hqToPath hq

                AbilityConstructorReference hq ->
                    "ability-constructors" :: hqToPath hq

                DataConstructorReference hq ->
                    "data-constructors" :: hqToPath hq

        perspectiveParamsToPath pp includeNamespacesSuffix =
            case pp of
                Perspective.ByRoot Perspective.Relative ->
                    [ "latest" ]

                Perspective.ByRoot (Perspective.Absolute hash) ->
                    [ Hash.toUrlString hash ]

                Perspective.ByNamespace Perspective.Relative fqn ->
                    if includeNamespacesSuffix then
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        "latest" :: "namespaces" :: NEL.toList (FQN.segments fqn)

                -- Currently the model supports Absolute URLs (aka Permalinks),
                -- but we don't use it since Unison Share does not support any
                -- history, meaning that everytime we deploy Unison Share, the
                -- previous versions of the codebase are lost.
                -- It's fully intended for this feature to be brought back
                Perspective.ByNamespace (Perspective.Absolute hash) fqn ->
                    if includeNamespacesSuffix then
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn) ++ [ namespaceSuffix ]

                    else
                        Hash.toUrlString hash :: "namespaces" :: NEL.toList (FQN.segments fqn)

        codePath codeRoute =
            case codeRoute of
                CodeRoot params ->
                    perspectiveParamsToPath params False

                Definition params ref ->
                    perspectiveParamsToPath params True ++ refPath ref

        ( path, queryParams ) =
            case route of
                Catalog ->
                    ( [ "catalog" ], [] )

                Account ->
                    ( [ "account" ], [] )

                User handle_ UserProfile ->
                    ( [ UserHandle.toString handle_ ], [] )

                User handle_ (UserCode codeRoute) ->
                    ( [ UserHandle.toString handle_, "p", "code" ] ++ codePath codeRoute, [] )

                User handle_ UserContributions ->
                    ( [ UserHandle.toString handle_, "p", "contributions" ], [] )

                Project projectRef_ ProjectOverview ->
                    ( ProjectRef.toUrlPath projectRef_, [] )

                Project projectRef_ ProjectBranches ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "branches" ], [] )

                Project projectRef_ (ProjectBranch branchRef vm codeRoute) ->
                    let
                        path_ =
                            ProjectRef.toUrlPath projectRef_
                                ++ "code"
                                :: BranchRef.toUrlPath branchRef
                                ++ codePath codeRoute
                    in
                    if ViewMode.isPresentation vm then
                        ( path_, [ string "viewMode" (ViewMode.toString vm) ] )

                    else
                        ( path_, [] )

                Project projectRef_ (ProjectRelease v) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "releases", Version.toString v ], [] )

                Project projectRef_ ProjectReleases ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "releases" ], [] )

                Project projectRef_ (ProjectContribution r ProjectContributionOverview) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions", ContributionRef.toUrlString r ], [] )

                Project projectRef_ (ProjectContribution r (ProjectContributionChanges Nothing)) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions", ContributionRef.toUrlString r, "changes" ], [] )

                Project projectRef_ (ProjectContribution r (ProjectContributionChanges (Just changeLineId))) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions", ContributionRef.toUrlString r, "changes", ChangeLineId.toString changeLineId ], [] )

                Project projectRef_ ProjectContributions ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions" ], [] )

                Project projectRef_ (ProjectTicket r) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "tickets", TicketRef.toUrlString r ], [] )

                Project projectRef_ ProjectTickets ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "tickets" ], [] )

                Project projectRef_ ProjectSettings ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "settings" ], [] )

                TermsOfService ->
                    ( [ "terms-of-service" ], [] )

                AcceptTerms (Just continueUrl) ->
                    ( [ "accept-terms" ], [ string "continue" (Url.toString continueUrl) ] )

                AcceptTerms Nothing ->
                    ( [ "accept-terms" ], [] )

                PrivacyPolicy ->
                    ( [ "privacy-policy" ], [] )

                UcmConnected ->
                    ( [ "ucm-connected" ], [] )

                Cloud ->
                    ( [ "cloud" ], [] )

                Error e ->
                    ( [ "error" ], [ string "appError" (AppError.toString e) ] )

                NotFound _ ->
                    ( [ "catalog" ], [] )
    in
    relative path queryParams



-- EFFECTS


navigate : Nav.Key -> Route -> Cmd msg
navigate navKey route =
    route
        |> toUrlString
        |> Nav.pushUrl navKey
