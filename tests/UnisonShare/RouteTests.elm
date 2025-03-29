module UnisonShare.RouteTests exposing (..)

import Code.BranchRef as BranchRef
import Code.Definition.Reference as Reference exposing (Reference)
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Perspective as Perspective
import Code.Version as Version
import Expect
import Lib.UserHandle as UserHandle
import Test exposing (..)
import UI.ViewMode as ViewMode
import UnisonShare.AppError as AppError
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Route as Route exposing (CodeRoute(..), ProjectRoute(..), Route(..), UserRoute(..))
import Url exposing (Url)


catalogRoute : Test
catalogRoute =
    describe "Route.fromUrl : catalog route"
        [ test "Matches /catalog to Catalog" <|
            \_ ->
                let
                    url =
                        mkUrl "/catalog"
                in
                Expect.equal Catalog (Route.fromUrl "" url)
        , test "Matches root to Catalog" <|
            \_ ->
                let
                    url =
                        mkUrl "/"
                in
                Expect.equal Catalog (Route.fromUrl "" url)
        ]


accountRoute : Test
accountRoute =
    describe "Route.fromUrl : account route"
        [ test "Matches /account to Account" <|
            \_ ->
                let
                    url =
                        mkUrl "/account"
                in
                Expect.equal Account (Route.fromUrl "" url)
        ]


termsOfServiceRoute : Test
termsOfServiceRoute =
    describe "Route.fromUrl : terms of service route"
        [ test "Matches /terms-of-service to TermsOfService" <|
            \_ ->
                let
                    url =
                        mkUrl "/terms-of-service"
                in
                Expect.equal TermsOfService (Route.fromUrl "" url)
        ]


privacyPolicyRoute : Test
privacyPolicyRoute =
    describe "Route.fromUrl : privacy policy route"
        [ test "Matches /privacy-policy to PrivacyPolicy" <|
            \_ ->
                let
                    url =
                        mkUrl "/privacy-policy"
                in
                Expect.equal PrivacyPolicy (Route.fromUrl "" url)
        ]


ucmConnectedRoute : Test
ucmConnectedRoute =
    describe "Route.fromUrl : ucm connected route"
        [ test "Matches /ucm-connected to UcmConnected" <|
            \_ ->
                let
                    url =
                        mkUrl "/ucm-connected"
                in
                Expect.equal UcmConnected (Route.fromUrl "" url)
        ]


cloudRoute : Test
cloudRoute =
    describe "Route.fromUrl : cloud route"
        [ test "Matches /cloud to Cloud" <|
            \_ ->
                let
                    url =
                        mkUrl "/cloud"
                in
                Expect.equal Cloud (Route.fromUrl "" url)
        ]


error : Test
error =
    describe "Route.fromUrl : error route"
        [ test "Matches /error?appError=UnspecifiedError to Error" <|
            \_ ->
                let
                    url =
                        mkUrl "/error?appError=UnspecifiedError"
                in
                Expect.equal (Error AppError.UnspecifiedError) (Route.fromUrl "" url)
        ]


notFound : Test
notFound =
    describe "Route.fromUrl : not found route"
        [ test "Matches /this-does-not-exist to NotFound" <|
            \_ ->
                let
                    url =
                        mkUrl "/this-does-not-exist"
                in
                Expect.equal (NotFound "/this-does-not-exist") (Route.fromUrl "" url)
        ]



-- USERS ROUTE


userProfileRoute : Test
userProfileRoute =
    describe "Route.fromUrl : user profile route"
        [ test "Matches /:handle to UserProfile " <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison") UserProfile)
                    (Route.fromUrl "" url)
        ]


userContributionsRoute : Test
userContributionsRoute =
    describe "Route.fromUrl : user contributions route"
        [ test "Matches /:handle/p/contributions to UserContributions " <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/p/contributions"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison") UserContributions)
                    (Route.fromUrl "" url)
        ]


userCodeRoute : Test
userCodeRoute =
    describe "Route.fromUrl : user code route"
        [ test "Matches /:handle/p/code/latest to UserCode" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/p/code/latest"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison")
                        (UserCode
                            (CodeRoot (Perspective.toParams Perspective.relativeRootPerspective))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/p/code/latest/namespaces/data/List to UserCode with a perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/p/code/latest/namespaces/data/List"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison")
                        (UserCode
                            (CodeRoot (Perspective.toParams (Perspective.namespacePerspective (fqn "data.List"))))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/p/code/latest/terms/data/List/map to UserCode with a definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/p/code/latest/terms/data/List/map"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison")
                        (UserCode
                            (Definition (Perspective.toParams Perspective.relativeRootPerspective) (termRef "data.List.map"))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/p/code/latest/namespaces/data/List/;/terms/map to UserCode with a perspective and definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/p/code/latest/namespaces/data/List/;/terms/map"
                in
                Expect.equal
                    (User (UserHandle.unsafeFromString "unison")
                        (UserCode
                            (Definition
                                (Perspective.toParams (Perspective.namespacePerspective (fqn "data.List")))
                                (termRef "map")
                            )
                        )
                    )
                    (Route.fromUrl "" url)
        ]



-- PROJECTS ROUTE


projectOverviewRoute : Test
projectOverviewRoute =
    describe "Route.fromUrl : project overview route"
        [ test "Matches /:handle/:slug to ProjectOverview " <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        ProjectOverview
                    )
                    (Route.fromUrl "" url)
        ]


projectBranchesRoute : Test
projectBranchesRoute =
    describe "Route.fromUrl : project branches route"
        [ test "Matches /:handle/:slug/branches to ProjectBranches " <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/branches"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        ProjectBranches
                    )
                    (Route.fromUrl "" url)
        ]


projectBranchRoute : Test
projectBranchRoute =
    describe "Route.fromUrl : project branch route"
        [ test "Matches /:handle/:slug/code/:branchRef to ProjectBranch" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/main"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "main")
                            ViewMode.Regular
                            (CodeRoot (Perspective.toParams Perspective.relativeRootPerspective))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/@user/branch to ProjectBranch with a contributor branch" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/@user/branch"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "user/branch")
                            ViewMode.Regular
                            (CodeRoot (Perspective.toParams Perspective.relativeRootPerspective))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/releases/drafts/1.2.3 to ProjectBranch with a release draft branch" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/releases/drafts/1.2.3"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "releases/drafts/1.2.3")
                            ViewMode.Regular
                            (CodeRoot (Perspective.toParams Perspective.relativeRootPerspective))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/releases/1.2.3 to ProjectBranch with a release branch" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/releases/1.2.3"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "releases/1.2.3")
                            ViewMode.Regular
                            (CodeRoot (Perspective.toParams Perspective.relativeRootPerspective))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/:branchRef/latest/namespaces/data/List to ProjectBranch with a perspective" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/main/latest/namespaces/data/List"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "main")
                            ViewMode.Regular
                            (CodeRoot (Perspective.toParams (Perspective.namespacePerspective (fqn "data.List"))))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/:branchRef/latest/terms/data/List/map to ProjectBranch with a definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/main/latest/terms/data/List/map"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "main")
                            ViewMode.Regular
                            (Definition (Perspective.toParams Perspective.relativeRootPerspective) (termRef "data.List.map"))
                        )
                    )
                    (Route.fromUrl "" url)
        , test "Matches /:handle/:slug/code/:branchRef/latest/namespaces/data/List/;/terms/map to ProjectBranch with a perspective and definition" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/code/main/latest/namespaces/data/List/;/terms/map"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectBranch
                            (BranchRef.unsafeFromString "main")
                            ViewMode.Regular
                            (Definition
                                (Perspective.toParams (Perspective.namespacePerspective (fqn "data.List")))
                                (termRef "map")
                            )
                        )
                    )
                    (Route.fromUrl "" url)
        ]


projectReleasesRoute : Test
projectReleasesRoute =
    describe "Route.fromUrl : project releases route"
        [ test "Matches /:handle/:slug/releases to ProjectReleases" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/releases"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        ProjectReleases
                    )
                    (Route.fromUrl "" url)
        ]


projectReleaseRoute : Test
projectReleaseRoute =
    describe "Route.fromUrl : project release route"
        [ test "Matches /:handle/:slug/releases/:version to ProjectRelease" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/releases/1.2.3"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        (ProjectRelease (Version.version 1 2 3))
                    )
                    (Route.fromUrl "" url)
        ]


projectSettingsRoute : Test
projectSettingsRoute =
    describe "Route.fromUrl : project settings route"
        [ test "Matches /:handle/:slug/settings to ProjectSettings" <|
            \_ ->
                let
                    url =
                        mkUrl "/@unison/base/settings"
                in
                Expect.equal
                    (Project (ProjectRef.unsafeFromString "unison" "base")
                        ProjectSettings
                    )
                    (Route.fromUrl "" url)
        ]



-- HELPERS


fqn : String -> FQN
fqn =
    FQN.fromString


termRef : String -> Reference
termRef str =
    Reference.fromString Reference.TermReference str


mkUrl : String -> Url
mkUrl path =
    { protocol = Url.Https
    , host = "unison-lang.org"
    , port_ = Just 443
    , path = path
    , query = Nothing
    , fragment = Nothing
    }
