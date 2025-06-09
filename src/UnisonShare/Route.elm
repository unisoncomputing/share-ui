module UnisonShare.Route exposing
    ( CodeRoute(..)
    , NotificationsRoute(..)
    , OrgRoute(..)
    , ProjectContributionRoute(..)
    , ProjectContributionsRoute(..)
    , ProjectRoute(..)
    , Route(..)
    , UserRoute(..)
    , acceptTerms
    , account
    , catalog
    , cloud
    , codeRoot
    , definition
    , finishSignup
    , fromUrl
    , navigate
    , notificationsAll
    , notificationsArchive
    , notificationsUnread
    , orgPeople
    , orgProfile
    , orgSettings
    , privacyPolicy
    , projectBranch
    , projectBranchDefinition
    , projectBranchRoot
    , projectBranches
    , projectContribution
    , projectContributionChange
    , projectContributionChanges
    , projectContributions
    , projectContributionsArchived
    , projectContributionsMerged
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
    , userContributions
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
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppError as AppError exposing (AppError)
import UnisonShare.BranchDiff.ChangeLineId as ChangeLineId exposing (ChangeLineId)
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Paginated as Paginated exposing (PageCursorParam(..))
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import Url exposing (Url)
import Url.Builder exposing (relative, string)


type CodeRoute
    = CodeRoot PerspectiveParams
    | Definition PerspectiveParams Reference


type UserRoute
    = UserContributions
    | UserCode


type OrgRoute
    = OrgPeople
    | OrgSettings


type ProjectContributionRoute
    = ProjectContributionOverview
    | ProjectContributionChanges (Maybe ChangeLineId)


type ProjectContributionsRoute
    = ProjectContributionsInReview
    | ProjectContributionsMerged
    | ProjectContributionsArchived


type ProjectRoute
    = ProjectOverview
    | ProjectBranches
    | ProjectBranch BranchRef CodeRoute
    | ProjectRelease Version
    | ProjectReleases
    | ProjectTicket TicketRef
    | ProjectTickets
    | ProjectContribution ContributionRef ProjectContributionRoute
    | ProjectContributions ProjectContributionsRoute
    | ProjectSettings


type NotificationsRoute
    = NotificationsAll PageCursorParam
    | NotificationsUnread PageCursorParam
    | NotificationsArchive PageCursorParam


type Route
    = Catalog
    | Account
    | Profile UserHandle
    | User UserHandle UserRoute
    | Org UserHandle OrgRoute
    | Project ProjectRef ProjectRoute
    | Notifications NotificationsRoute
    | TermsOfService
    | AcceptTerms (Maybe Url)
    | PrivacyPolicy
    | UcmConnected
    | Cloud
    | NotFound String
    | Error AppError
    | FinishSignup UserHandle String



-- CREATE ---------------------------------------------------------------------


catalog : Route
catalog =
    Catalog


account : Route
account =
    Account


notificationsAll : PageCursorParam -> Route
notificationsAll cursorParam =
    Notifications (NotificationsAll cursorParam)


notificationsUnread : PageCursorParam -> Route
notificationsUnread cursorParam =
    Notifications (NotificationsUnread cursorParam)


notificationsArchive : PageCursorParam -> Route
notificationsArchive cursorParam =
    Notifications (NotificationsArchive cursorParam)


cloud : Route
cloud =
    Cloud


userProfile : UserHandle -> Route
userProfile handle_ =
    Profile handle_


orgProfile : UserHandle -> Route
orgProfile handle_ =
    Profile handle_


orgPeople : UserHandle -> Route
orgPeople handle_ =
    Org handle_ OrgPeople


orgSettings : UserHandle -> Route
orgSettings handle_ =
    Org handle_ OrgSettings


userCode : UserHandle -> Route
userCode handle_ =
    User handle_ UserCode


userContributions : UserHandle -> Route
userContributions handle_ =
    User handle_ UserContributions


projectOverview : ProjectRef -> Route
projectOverview projectRef_ =
    Project projectRef_ ProjectOverview


projectBranch : ProjectRef -> BranchRef -> CodeRoute -> Route
projectBranch projectRef_ branchRef_ codeRoute =
    Project projectRef_ (ProjectBranch branchRef_ codeRoute)


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
    Project projectRef_ (ProjectContributions ProjectContributionsInReview)


projectContributionsMerged : ProjectRef -> Route
projectContributionsMerged projectRef_ =
    Project projectRef_ (ProjectContributions ProjectContributionsMerged)


projectContributionsArchived : ProjectRef -> Route
projectContributionsArchived projectRef_ =
    Project projectRef_ (ProjectContributions ProjectContributionsArchived)


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
    Project projectRef_ (ProjectBranch branchRef_ (Definition pp ref))


projectBranchRoot : ProjectRef -> BranchRef -> Perspective -> Route
projectBranchRoot projectRef_ branchRef pers =
    let
        pp =
            Perspective.toParams pers
    in
    Project projectRef_ (ProjectBranch branchRef (CodeRoot pp))


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


finishSignup : UserHandle -> String -> Route
finishSignup handle_ state =
    FinishSignup handle_ state



-- PARSE ----------------------------------------------------------------------


toRoute : Maybe String -> Parser Route
toRoute queryString =
    oneOf
        [ b homeParser
        , b catalogParser
        , b accountParser
        , b (notificationsParser queryString)
        , b profileParser
        , b userParser
        , b orgParser
        , b projectParser -- Specifically comes _after_ userParser because project slugs share the url space with user pages
        , b termsOfServiceParser
        , b (acceptTermsParser queryString)
        , b privacyPolicyParser
        , b ucmConnectedParser
        , b (finishSignupParser queryString)
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


notificationsParser : Maybe String -> Parser Route
notificationsParser queryString =
    let
        cursorParser =
            Parser.chompUntilEndOr "&"
                |> Parser.getChompedString
                |> Parser.map Paginated.PageCursor

        paginationCursorParser =
            oneOf
                [ b (succeed PrevPage |. s "prev" |. symbol "=" |= cursorParser |. end)
                , b (succeed NextPage |. s "next" |. symbol "=" |= cursorParser |. end)
                ]

        paginationCursor =
            queryString
                |> Maybe.withDefault ""
                |> Parser.run paginationCursorParser
                |> Result.withDefault NoPageCursor
    in
    oneOf
        [ b (succeed (Notifications (NotificationsAll paginationCursor)) |. slash |. s "notifications" |. slash |. s "all")
        , b (succeed (Notifications (NotificationsUnread paginationCursor)) |. slash |. s "notifications" |. slash |. s "unread")
        , b (succeed (Notifications (NotificationsArchive paginationCursor)) |. slash |. s "notifications" |. slash |. s "archive")
        , b (succeed (Notifications (NotificationsAll paginationCursor)) |. slash |. s "notifications")
        ]


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


finishSignupParser : Maybe String -> Parser Route
finishSignupParser queryString =
    let
        userHandleQueryParser : Parser UserHandle
        userHandleQueryParser =
            let
                parseMaybe mhandle =
                    case mhandle of
                        Just u ->
                            Parser.succeed u

                        Nothing ->
                            Parser.problem "Invalid handle"
            in
            Parser.chompUntilEndOr "&"
                |> Parser.getChompedString
                |> Parser.map UserHandle.fromUnprefixedString
                |> Parser.andThen parseMaybe

        stateQueryParser : Parser String
        stateQueryParser =
            Parser.chompUntilEndOr "&"
                |> Parser.getChompedString

        finishSignupQueryParser : Parser ( UserHandle, String )
        finishSignupQueryParser =
            oneOf
                [ b (succeed (\h s -> ( h, s )) |. s "conflictingHandle" |. symbol "=" |= userHandleQueryParser |. symbol "&" |. s "state" |. symbol "=" |= stateQueryParser)
                , b (succeed (\s h -> ( h, s )) |. s "state" |. symbol "=" |= stateQueryParser |. symbol "&" |. s "conflictingHandle" |. symbol "=" |= userHandleQueryParser)
                ]
    in
    queryString
        |> Maybe.withDefault ""
        |> Parser.run finishSignupQueryParser
        |> Result.map (\( handle, state ) -> succeed (FinishSignup handle state) |. slash |. s "finish-signup")
        |> Result.withDefault (Parser.problem "Missing handle or state in querystring")


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


profileParser : Parser Route
profileParser =
    b (succeed Profile |. slash |= userHandle |. end)


userParser : Parser Route
userParser =
    let
        userCode_ h =
            User h UserCode

        userContrib h =
            User h UserContributions
    in
    oneOf
        [ b (succeed userContrib |. slash |= userHandle |. slash |. s "p" |. slash |. s "contributions" |. end)
        , b (succeed userCode_ |. slash |= userHandle |. slash |. s "code" |. end)
        , b (succeed userCode_ |. slash |= userHandle |. slash |. s "p" |. slash |. s "code" |. end)
        ]


orgParser : Parser Route
orgParser =
    let
        orgPeople_ h =
            Org h OrgPeople

        orgSettings_ h =
            Org h OrgSettings
    in
    oneOf
        [ b (succeed orgPeople_ |. slash |= userHandle |. slash |. s "p" |. slash |. s "people" |. end)
        , b (succeed orgSettings_ |. slash |= userHandle |. slash |. s "p" |. slash |. s "settings" |. end)
        ]


projectParser : Parser Route
projectParser =
    let
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
            Project ps (ProjectBranch branchRef c)

        projectBranchRoot_ handle slug branchRef =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectBranch branchRef (CodeRoot (Perspective.ByRoot Perspective.Relative)))

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

        projectContributions_ subRoute handle slug =
            let
                ps =
                    ProjectRef.projectRef handle slug
            in
            Project ps (ProjectContributions subRoute)

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
        , b (succeed (projectContributions_ ProjectContributionsInReview) |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. end)
        , b (succeed (projectContributions_ ProjectContributionsMerged) |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. slash |. s "merged" |. end)
        , b (succeed (projectContributions_ ProjectContributionsArchived) |. slash |= userHandle |. slash |= projectSlug |. slash |. s "contributions" |. slash |. s "archived" |. end)
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

        Notifications (NotificationsAll _) ->
            "notifications"

        Notifications (NotificationsUnread _) ->
            "notifications/unread"

        Notifications (NotificationsArchive _) ->
            "notifications/archive"

        Profile _ ->
            ":handle"

        User _ UserCode ->
            ":handle/p/code"

        User _ UserContributions ->
            ":handle/p/contributions"

        Org _ OrgPeople ->
            ":handle/p/people"

        Org _ OrgSettings ->
            ":handle/p/settings"

        Project _ ProjectOverview ->
            ":handle/:project-slug"

        Project _ ProjectBranches ->
            ":handle/:project-slug/branches"

        Project _ (ProjectBranch _ codeRoute) ->
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

        Project _ (ProjectContributions ProjectContributionsInReview) ->
            ":handle/:project-slug/contributions"

        Project _ (ProjectContributions ProjectContributionsMerged) ->
            ":handle/:project-slug/contributions/merged"

        Project _ (ProjectContributions ProjectContributionsArchived) ->
            ":handle/:project-slug/contributions/archived"

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

        FinishSignup _ _ ->
            "finish-signup"

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

        paginationCursorToQueryParams cursor =
            case cursor of
                NoPageCursor ->
                    []

                PrevPage (Paginated.PageCursor cursor_) ->
                    [ string "prev" cursor_ ]

                NextPage (Paginated.PageCursor cursor_) ->
                    [ string "next" cursor_ ]

        ( path, queryParams ) =
            case route of
                Catalog ->
                    ( [ "catalog" ], [] )

                Account ->
                    ( [ "account" ], [] )

                Notifications (NotificationsAll cursor) ->
                    ( [ "notifications" ], paginationCursorToQueryParams cursor )

                Notifications (NotificationsUnread cursor) ->
                    ( [ "notifications", "unread" ], paginationCursorToQueryParams cursor )

                Notifications (NotificationsArchive cursor) ->
                    ( [ "notifications", "archive" ], paginationCursorToQueryParams cursor )

                Profile handle_ ->
                    ( [ UserHandle.toString handle_ ], [] )

                User handle_ UserCode ->
                    ( [ UserHandle.toString handle_, "p", "code" ], [] )

                User handle_ UserContributions ->
                    ( [ UserHandle.toString handle_, "p", "contributions" ], [] )

                Org handle_ OrgPeople ->
                    ( [ UserHandle.toString handle_, "p", "people" ], [] )

                Org handle_ OrgSettings ->
                    ( [ UserHandle.toString handle_, "p", "settings" ], [] )

                Project projectRef_ ProjectOverview ->
                    ( ProjectRef.toUrlPath projectRef_, [] )

                Project projectRef_ ProjectBranches ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "branches" ], [] )

                Project projectRef_ (ProjectBranch branchRef codeRoute) ->
                    let
                        path_ =
                            ProjectRef.toUrlPath projectRef_
                                ++ "code"
                                :: BranchRef.toUrlPath branchRef
                                ++ codePath codeRoute
                    in
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

                Project projectRef_ (ProjectContributions ProjectContributionsInReview) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions" ], [] )

                Project projectRef_ (ProjectContributions ProjectContributionsMerged) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions", "merged" ], [] )

                Project projectRef_ (ProjectContributions ProjectContributionsArchived) ->
                    ( ProjectRef.toUrlPath projectRef_ ++ [ "contributions", "archived" ], [] )

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

                FinishSignup handle state ->
                    ( [ "finish-signup" ], [ string "handle" (UserHandle.toUnprefixedString handle), string "state" state ] )

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
