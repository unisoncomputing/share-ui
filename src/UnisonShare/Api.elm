module UnisonShare.Api exposing
    ( NewProjectContribution
    , NewProjectTicket
    , ProjectBranchesKindFilter(..)
    , ProjectBranchesParams
    , ProjectContributionUpdate(..)
    , ProjectTicketUpdate(..)
    , ProjectUpdate(..)
    , UserBranchesParams
    , browseCodebase
    , catalog
    , codebaseApiEndpointToEndpoint
    , completeTours
    , createProjectContribution
    , createProjectContributionComment
    , createProjectRelease
    , createProjectTicket
    , createProjectTicketComment
    , createSupportTicket
    , deleteProject
    , deleteProjectBranch
    , deleteProjectContributionComment
    , deleteProjectTicketComment
    , namespace
    , project
    , projectBranch
    , projectBranchDefinitionDiff
    , projectBranchDiff
    , projectBranchReleaseNotes
    , projectBranches
    , projectContribution
    , projectContributionDiff
    , projectContributionTimeline
    , projectContributions
    , projectReadme
    , projectRelease
    , projectReleaseNotes
    , projectReleases
    , projectTicket
    , projectTicketTimeline
    , projectTickets
    , projects
    , search
    , session
    , updateProject
    , updateProjectContribution
    , updateProjectContributionComment
    , updateProjectFav
    , updateProjectTicket
    , updateProjectTicketComment
    , updateUserProfile
    , user
    , userBranches
    , userProjects
    , userReadme
    )

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.CodebaseApi as CodebaseApi
import Code.Definition.Reference as Reference exposing (Reference(..))
import Code.FullyQualifiedName as FQN exposing (FQN)
import Code.Hash as Hash exposing (Hash)
import Code.HashQualified as HQ
import Code.Namespace.NamespaceRef as NamespaceRef exposing (NamespaceRef)
import Code.Perspective as Perspective exposing (Perspective(..))
import Code.Syntax as Syntax
import Code.Version as Version exposing (Version)
import Http
import Json.Encode as Encode
import Json.Encode.Extra as EncodeE
import Lib.HttpApi exposing (Endpoint(..))
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Maybe.Extra as MaybeE
import Regex
import Set exposing (Set)
import UI.KeyboardShortcut.Key exposing (Key(..))
import UnisonShare.CodeBrowsingContext exposing (CodeBrowsingContext(..))
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.DefinitionDiffKey exposing (DefinitionDiffKey(..))
import UnisonShare.Project as Project exposing (ProjectVisibility)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)
import UnisonShare.Timeline.CommentId as CommentId exposing (CommentId)
import UnisonShare.Tour as Tour exposing (Tour)
import Url.Builder exposing (QueryParameter, int, string)


user : UserHandle -> Endpoint
user handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle ], queryParams = [] }


updateUserProfile : UserHandle -> { bio : String } -> Endpoint
updateUserProfile handle profile =
    let
        body =
            Encode.object [ ( "bio", Encode.string profile.bio ) ]
                |> Http.jsonBody
    in
    PATCH
        { path = [ "users", UserHandle.toUnprefixedString handle ]
        , queryParams = []
        , body = body
        }


userReadme : UserHandle -> Endpoint
userReadme handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle, "readme" ], queryParams = [] }


type alias UserBranchesParams =
    { searchQuery : Maybe String
    , projectRef : Maybe ProjectRef
    , limit : Int
    , cursor : Maybe String
    }


userBranches : UserHandle -> UserBranchesParams -> Endpoint
userBranches handle params =
    let
        queryParams =
            int "limit" params.limit
                :: (params.cursor
                        |> Maybe.map (string "cursor")
                        |> MaybeE.toList
                   )
                ++ (params.searchQuery
                        |> Maybe.map (string "name-prefix")
                        |> MaybeE.toList
                   )
                ++ (params.projectRef
                        |> Maybe.map (ProjectRef.toString >> string "project-ref")
                        |> MaybeE.toList
                   )
    in
    GET
        { path = [ "users", UserHandle.toUnprefixedString handle, "branches" ]
        , queryParams = queryParams
        }


userProjects : UserHandle -> Endpoint
userProjects handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle, "projects" ], queryParams = [] }


search : String -> Endpoint
search query =
    GET { path = [ "search" ], queryParams = [ string "query" query ] }


completeTours : List Tour -> Endpoint
completeTours tours =
    let
        body =
            tours
                |> List.map Tour.toString
                |> Encode.list Encode.string
                |> Http.jsonBody
    in
    POST
        { path = [ "account", "tours" ]
        , queryParams = []
        , body = body
        }


createSupportTicket :
    { subject : String, body : String, tags : List String }
    -> Endpoint
createSupportTicket data =
    let
        body =
            Encode.object
                [ ( "subject", Encode.string data.subject )
                , ( "body", Encode.string data.body )
                , ( "priority", Encode.string "normal" )
                , ( "tags", Encode.list Encode.string data.tags )
                ]
    in
    POST { path = [ "support", "tickets" ], queryParams = [], body = Http.jsonBody body }


{-| TODO:
Should likely be a /session endpoint

Instead, for now we're using the /account endpoint, but over time that endpoint
is likely to grow to include data not needed for Session.

-}
session : Endpoint
session =
    GET { path = [ "account" ], queryParams = [] }



-- PROJECTS


project : ProjectRef -> Endpoint
project projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET { path = [ "users", handle, "projects", slug ], queryParams = [] }


type ProjectBranchesKindFilter
    = AllBranches (Maybe UserHandle)
    | ContributorBranches (Maybe UserHandle)
    | ProjectBranches


type alias ProjectBranchesParams =
    { kind : ProjectBranchesKindFilter
    , searchQuery : Maybe String
    , limit : Int
    , cursor : Maybe String
    }


projectBranches : ProjectRef -> ProjectBranchesParams -> Endpoint
projectBranches projectRef params =
    let
        ( kind, contributorHandleParams ) =
            case params.kind of
                AllBranches Nothing ->
                    ( string "kind" "all", [] )

                AllBranches (Just h) ->
                    ( string "kind" "all", [ h |> UserHandle.toString |> string "contributor-handle" ] )

                ContributorBranches Nothing ->
                    ( string "kind" "contributor", [] )

                ContributorBranches (Just h) ->
                    ( string "kind" "contributor", [ h |> UserHandle.toString |> string "contributor-handle" ] )

                ProjectBranches ->
                    ( string "kind" "core", [] )

        queryParams =
            [ kind, int "limit" params.limit ]
                ++ (params.cursor
                        |> Maybe.map (string "cursor")
                        |> MaybeE.toList
                   )
                ++ (params.searchQuery
                        |> Maybe.map (string "name-prefix")
                        |> MaybeE.toList
                   )
                ++ contributorHandleParams

        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "branches" ]
        , queryParams = queryParams
        }


projectBranch : ProjectRef -> BranchRef -> Endpoint
projectBranch projectRef branchRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "branches", BranchRef.toApiUrlString branchRef ]
        , queryParams = []
        }


projectBranchReleaseNotes : ProjectRef -> BranchRef -> Endpoint
projectBranchReleaseNotes projectRef branchRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "branches"
            , BranchRef.toApiUrlString branchRef
            , "releaseNotes"
            ]
        , queryParams = []
        }



-- PROJECT CONTRIBUTIONS


projectContribution : ProjectRef -> ContributionRef -> Endpoint
projectContribution projectRef contribRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef ]
        , queryParams = []
        }


type alias NewProjectContribution =
    { title : String
    , description : Maybe String
    , status : ContributionStatus
    , sourceBranchRef : BranchRef
    , targetBranchRef : BranchRef
    }


createProjectContribution : ProjectRef -> NewProjectContribution -> Endpoint
createProjectContribution projectRef data =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "title", Encode.string data.title )
                , ( "description", MaybeE.unwrap Encode.null Encode.string data.description )
                , ( "status", Encode.string (ContributionStatus.toApiString data.status) )
                , ( "sourceBranchRef", Encode.string (BranchRef.toString data.sourceBranchRef) )
                , ( "targetBranchRef", Encode.string (BranchRef.toString data.targetBranchRef) )
                ]
    in
    POST
        { path = [ "users", handle, "projects", slug, "contributions" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


createProjectContributionComment : ProjectRef -> ContributionRef -> String -> Endpoint
createProjectContributionComment projectRef contribRef comment =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "content", Encode.string comment )
                ]
    in
    POST
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "contributions"
            , ContributionRef.toApiString contribRef
            , "timeline"
            , "comments"
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


updateProjectContributionComment : ProjectRef -> ContributionRef -> CommentId -> Int -> String -> Endpoint
updateProjectContributionComment projectRef contribRef commentId originalRevision comment =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "content", Encode.string comment )
                , ( "expectedRevision", Encode.int originalRevision )
                ]
    in
    PATCH
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "contributions"
            , ContributionRef.toApiString contribRef
            , "timeline"
            , "comments"
            , CommentId.toString commentId
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


deleteProjectContributionComment : ProjectRef -> ContributionRef -> CommentId -> Endpoint
deleteProjectContributionComment projectRef contribRef commentId =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    DELETE
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "contributions"
            , ContributionRef.toApiString contribRef
            , "timeline"
            , "comments"
            , CommentId.toString commentId
            ]
        , queryParams = []
        }


type ProjectContributionUpdate
    = ProjectContributionUpdate
        { title : String
        , description : Maybe String
        , status : ContributionStatus
        , sourceBranchRef : BranchRef
        , targetBranchRef : BranchRef
        }
    | ProjectContributionTitleUpdate String
    | ProjectContributionDescriptionUpdate String
    | ProjectContributionStatusUpdate ContributionStatus


updateProjectContribution : ProjectRef -> ContributionRef -> ProjectContributionUpdate -> Endpoint
updateProjectContribution projectRef contribRef update =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            case update of
                ProjectContributionUpdate update_ ->
                    Encode.object
                        [ ( "title", Encode.string update_.title )
                        , ( "description", MaybeE.unwrap Encode.null Encode.string update_.description )
                        , ( "status", Encode.string (ContributionStatus.toApiString update_.status) )
                        , ( "sourceBranchRef", Encode.string (BranchRef.toString update_.sourceBranchRef) )
                        , ( "targetBranchRef", Encode.string (BranchRef.toString update_.targetBranchRef) )
                        ]

                ProjectContributionTitleUpdate title ->
                    Encode.object
                        [ ( "title", Encode.string title )
                        ]

                ProjectContributionDescriptionUpdate description ->
                    Encode.object
                        [ ( "description", Encode.string description )
                        ]

                ProjectContributionStatusUpdate status ->
                    Encode.object
                        [ ( "status", Encode.string (ContributionStatus.toApiString status) )
                        ]
    in
    PATCH
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "contributions"
            , ContributionRef.toApiString contribRef
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


projectContributionTimeline : ProjectRef -> ContributionRef -> Endpoint
projectContributionTimeline projectRef contribRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef, "timeline" ]
        , queryParams = []
        }


projectContributions : ProjectRef -> Endpoint
projectContributions projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions" ]
        , queryParams = []
        }


projectContributionDiff : ProjectRef -> ContributionRef -> Endpoint
projectContributionDiff projectRef contribRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef, "diff" ]
        , queryParams = []
        }


projectBranchDefinitionDiff : ProjectRef -> DefinitionDiffKey -> Endpoint
projectBranchDefinitionDiff projectRef params =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        queryParams =
            case params of
                Term { branchA, branchB, definitionA, definitionB } ->
                    [ string "oldBranchRef" (BranchRef.toString branchA)
                    , string "newBranchRef" (BranchRef.toString branchB)
                    , string "oldTerm" (FQN.toApiUrlString definitionA)
                    , string "newTerm" (FQN.toApiUrlString definitionB)
                    ]

                Type { branchA, branchB, definitionA, definitionB } ->
                    [ string "oldBranchRef" (BranchRef.toString branchA)
                    , string "newBranchRef" (BranchRef.toApiUrlString branchB)
                    , string "oldType" (FQN.toApiUrlString definitionA)
                    , string "newType" (FQN.toApiUrlString definitionB)
                    ]
    in
    GET
        { path = [ "users", handle, "projects", slug, "diff", "terms" ]
        , queryParams = queryParams
        }


projectBranchDiff : ProjectRef -> BranchRef -> BranchRef -> Endpoint
projectBranchDiff projectRef branchA branchB =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        queryParams =
            [ string "from" (BranchRef.toApiUrlString branchA)
            , string "to" (BranchRef.toApiUrlString branchB)
            ]
    in
    GET
        { path = [ "users", handle, "projects", slug, "diff", "branches" ]
        , queryParams = queryParams
        }



-- PROJECT TICKETS


projectTicket : ProjectRef -> TicketRef -> Endpoint
projectTicket projectRef ticketRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "tickets", TicketRef.toApiString ticketRef ]
        , queryParams = []
        }


type alias NewProjectTicket =
    { title : String
    , description : String
    , status : TicketStatus
    }


createProjectTicket : ProjectRef -> NewProjectTicket -> Endpoint
createProjectTicket projectRef data =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "title", Encode.string data.title )
                , ( "description", Encode.string data.description )
                , ( "status", Encode.string (TicketStatus.toApiString data.status) )
                ]
    in
    POST
        { path = [ "users", handle, "projects", slug, "tickets" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


createProjectTicketComment : ProjectRef -> TicketRef -> String -> Endpoint
createProjectTicketComment projectRef ticketRef comment =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "content", Encode.string comment )
                ]
    in
    POST
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "tickets"
            , TicketRef.toApiString ticketRef
            , "timeline"
            , "comments"
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


updateProjectTicketComment : ProjectRef -> TicketRef -> CommentId -> Int -> String -> Endpoint
updateProjectTicketComment projectRef ticketRef commentId originalRevision comment =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "content", Encode.string comment )
                , ( "expectedRevision", Encode.int originalRevision )
                ]
    in
    PATCH
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "tickets"
            , TicketRef.toApiString ticketRef
            , "timeline"
            , "comments"
            , CommentId.toString commentId
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


deleteProjectTicketComment : ProjectRef -> TicketRef -> CommentId -> Endpoint
deleteProjectTicketComment projectRef ticketRef commentId =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    DELETE
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "tickets"
            , TicketRef.toApiString ticketRef
            , "timeline"
            , "comments"
            , CommentId.toString commentId
            ]
        , queryParams = []
        }


type ProjectTicketUpdate
    = ProjectTicketUpdate
        { title : String
        , description : String
        , status : TicketStatus
        }
    | ProjectTicketTitleUpdate String
    | ProjectTicketDescriptionUpdate String
    | ProjectTicketStatusUpdate TicketStatus


updateProjectTicket : ProjectRef -> TicketRef -> ProjectTicketUpdate -> Endpoint
updateProjectTicket projectRef ticketRef update =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            case update of
                ProjectTicketUpdate update_ ->
                    Encode.object
                        [ ( "title", Encode.string update_.title )
                        , ( "description", Encode.string update_.description )
                        , ( "status", Encode.string (TicketStatus.toApiString update_.status) )
                        ]

                ProjectTicketTitleUpdate title ->
                    Encode.object
                        [ ( "title", Encode.string title )
                        ]

                ProjectTicketDescriptionUpdate description ->
                    Encode.object
                        [ ( "description", Encode.string description )
                        ]

                ProjectTicketStatusUpdate status ->
                    Encode.object
                        [ ( "status", Encode.string (TicketStatus.toApiString status) )
                        ]
    in
    PATCH
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "tickets"
            , TicketRef.toApiString ticketRef
            ]
        , queryParams = []
        , body = Http.jsonBody body
        }


projectTicketTimeline : ProjectRef -> TicketRef -> Endpoint
projectTicketTimeline projectRef ticketRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "tickets"
            , TicketRef.toApiString ticketRef
            , "timeline"
            ]
        , queryParams = []
        }


projectTickets : ProjectRef -> Endpoint
projectTickets projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "tickets" ]
        , queryParams = []
        }



-- PROJECT RELEASES


projectRelease : ProjectRef -> Version -> Endpoint
projectRelease projectRef version =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "releases", Version.toString version ]
        , queryParams = []
        }


projectReleases : ProjectRef -> Endpoint
projectReleases projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "releases" ]
        , queryParams = []
        }


projectReleaseNotes : ProjectRef -> Version -> Endpoint
projectReleaseNotes projectRef version =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path =
            [ "users"
            , handle
            , "projects"
            , slug
            , "releases"
            , Version.toString version
            , "releaseNotes"
            ]
        , queryParams = []
        }


createProjectRelease :
    ProjectRef
    -> { branchRef : BranchRef, causalHash : Hash, version : Version }
    -> Endpoint
createProjectRelease projectRef data =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "branchRef", Encode.string (BranchRef.toString data.branchRef) )
                , ( "causalHash", Encode.string (Hash.toString data.causalHash) )
                , ( "major", Encode.int (Version.major data.version) )
                , ( "minor", Encode.int (Version.minor data.version) )
                , ( "patch", Encode.int (Version.patch data.version) )
                ]
    in
    POST
        { path = [ "users", handle, "projects", slug, "releases" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


updateProjectFav : ProjectRef -> Bool -> Endpoint
updateProjectFav projectRef isFaved =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object [ ( "isFaved", Encode.bool isFaved ) ]
                |> Http.jsonBody
    in
    PUT
        { path = [ "users", handle, "projects", slug, "fav" ]
        , queryParams = []
        , body = body
        }


type ProjectUpdate
    = ProjectDescriptionUpdate { summary : Maybe String, tags : Set String }
    | ProjectSettingsUpdate { visibility : ProjectVisibility }


updateProject : ProjectRef -> ProjectUpdate -> Endpoint
updateProject projectRef update =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            case update of
                ProjectDescriptionUpdate changes ->
                    Encode.object
                        [ ( "summary", EncodeE.maybe Encode.string changes.summary )
                        , ( "tags"
                          , Encode.object
                                [ ( "replaceWith"
                                  , Encode.list Encode.string (Set.toList changes.tags)
                                  )
                                ]
                          )
                        ]
                        |> Http.jsonBody

                ProjectSettingsUpdate changes ->
                    Encode.object
                        [ ( "visibility"
                          , Encode.string (Project.visibilityToString changes.visibility)
                          )
                        ]
                        |> Http.jsonBody
    in
    PATCH
        { path = [ "users", handle, "projects", slug ]
        , queryParams = []
        , body = body
        }


deleteProject : ProjectRef -> Endpoint
deleteProject projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    DELETE
        { path = [ "users", handle, "projects", slug ]
        , queryParams = []
        }


deleteProjectBranch : ProjectRef -> BranchRef -> Endpoint
deleteProjectBranch projectRef branchRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    DELETE
        { path = [ "users", handle, "projects", slug, "branches", BranchRef.toApiUrlString branchRef ]
        , queryParams = []
        }


projectReadme : ProjectRef -> Endpoint
projectReadme projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET { path = [ "users", handle, "projects", slug, "readme" ], queryParams = [] }


projects : Maybe String -> Endpoint
projects owner =
    let
        queryParams =
            case owner of
                Just owner_ ->
                    [ string "owner" owner_ ]

                Nothing ->
                    []
    in
    GET { path = [ "projects" ], queryParams = queryParams }


catalog : Endpoint
catalog =
    GET { path = [ "catalog" ], queryParams = [] }



-- CODE BROWSING


baseCodePathFromContext : CodeBrowsingContext -> List String
baseCodePathFromContext context =
    case context of
        UserCode h ->
            [ "codebases", UserHandle.toUnprefixedString h ]

        ProjectBranch pr br ->
            let
                ( handle, slug ) =
                    ProjectRef.toApiStringParts pr
            in
            case br of
                BranchRef.ReleaseBranchRef v ->
                    [ "users", handle, "projects", slug, "releases", Version.toString v ]

                _ ->
                    [ "users", handle, "projects", slug, "branches", BranchRef.toApiUrlString br ]


namespace : CodeBrowsingContext -> Perspective -> FQN -> Endpoint
namespace context perspective fqn =
    let
        queryParams =
            [ toRootHash (Perspective.rootPerspective perspective) ]

        base =
            baseCodePathFromContext context
    in
    GET
        { path = base ++ [ "namespaces", "by-name", FQN.toApiUrlString fqn ]
        , queryParams = MaybeE.values queryParams
        }


browseCodebase : CodeBrowsingContext -> Perspective -> Maybe NamespaceRef -> Endpoint
browseCodebase context perspective ref =
    let
        base =
            baseCodePathFromContext context

        namespace_ =
            ref
                |> Maybe.map NamespaceRef.toString
                |> Maybe.map (string "namespace")
                |> Maybe.map (\qp -> [ qp ])
    in
    GET
        { path = base ++ [ "browse" ]
        , queryParams = Maybe.withDefault [] namespace_ ++ perspectiveToQueryParams perspective
        }


codebaseApiEndpointToEndpoint : CodeBrowsingContext -> CodebaseApi.CodebaseEndpoint -> Endpoint
codebaseApiEndpointToEndpoint context cbEndpoint =
    let
        base =
            baseCodePathFromContext context
    in
    case cbEndpoint of
        CodebaseApi.Find { perspective, withinFqn, limit, sourceWidth, query } ->
            let
                params =
                    case withinFqn of
                        Just fqn ->
                            [ toRootHash (Perspective.rootPerspective perspective)
                            , Just (relativeTo fqn)
                            ]
                                |> MaybeE.values

                        Nothing ->
                            perspectiveToQueryParams perspective

                width =
                    case sourceWidth of
                        Syntax.Width w ->
                            w
            in
            GET
                { path = base ++ [ "find" ]
                , queryParams =
                    [ int "limit" limit
                    , int "renderWidth" width
                    , string "query" query
                    ]
                        ++ params
                }

        CodebaseApi.Browse { perspective, ref } ->
            browseCodebase context perspective ref

        CodebaseApi.Definition { perspective, ref } ->
            let
                -- TODO: Temporarily disable constructor suffixes in hashes
                constructorSuffixRegex =
                    Maybe.withDefault Regex.never (Regex.fromString "@[ad]\\d$")

                withoutConstructorSuffix h =
                    h
                        |> Hash.toApiUrlString
                        |> Regex.replace constructorSuffixRegex (always "")

                path =
                    case Reference.hashQualified ref of
                        HQ.NameOnly fqn ->
                            [ "definitions", "by-name", FQN.toApiUrlString fqn ]

                        HQ.HashOnly h ->
                            [ "definitions", "by-hash", withoutConstructorSuffix h ]

                        HQ.HashQualified _ h ->
                            [ "definitions", "by-hash", withoutConstructorSuffix h ]
            in
            GET
                { path = base ++ path
                , queryParams = perspectiveToQueryParams perspective
                }

        CodebaseApi.Summary { perspective, ref } ->
            let
                hqUrl hq =
                    case hq of
                        HQ.NameOnly fqn ->
                            -- TODO: Not really valid...
                            { path = [ "by-name", FQN.toApiUrlString fqn ]
                            , queryParams = []
                            }

                        HQ.HashOnly h ->
                            { path = [ "by-hash", Hash.toApiUrlString h ]
                            , queryParams = []
                            }

                        HQ.HashQualified fqn h ->
                            { path = [ "by-hash", Hash.toApiUrlString h ]
                            , queryParams = [ FQN.toQueryString fqn ]
                            }

                { path, queryParams } =
                    case ref of
                        Reference.TermReference hq ->
                            let
                                hqUrl_ =
                                    hqUrl hq
                            in
                            { hqUrl_ | path = [ "definitions", "terms" ] ++ hqUrl_.path ++ [ "summary" ] }

                        Reference.TypeReference hq ->
                            let
                                hqUrl_ =
                                    hqUrl hq
                            in
                            { hqUrl_ | path = [ "definitions", "types" ] ++ hqUrl_.path ++ [ "summary" ] }

                        Reference.AbilityConstructorReference hq ->
                            let
                                hqUrl_ =
                                    hqUrl hq
                            in
                            { hqUrl_ | path = [ "definitions", "terms" ] ++ hqUrl_.path ++ [ "summary" ] }

                        Reference.DataConstructorReference hq ->
                            let
                                hqUrl_ =
                                    hqUrl hq
                            in
                            { hqUrl_ | path = [ "definitions", "terms" ] ++ hqUrl_.path ++ [ "summary" ] }
            in
            GET
                { path = base ++ path
                , queryParams = queryParams ++ perspectiveToQueryParams perspective
                }



-- QUERY PARAMS ---------------------------------------------------------------


perspectiveToQueryParams : Perspective -> List QueryParameter
perspectiveToQueryParams perspective =
    case perspective of
        Root p ->
            MaybeE.values [ toRootHash p.root ]

        Namespace d ->
            [ toRootHash d.root, Just (relativeTo d.fqn) ] |> MaybeE.values


toRootHash : Perspective.RootPerspective -> Maybe QueryParameter
toRootHash rootPerspective =
    case rootPerspective of
        Perspective.Relative ->
            Nothing

        Perspective.Absolute h ->
            Just (rootHash h)


rootHash : Hash -> QueryParameter
rootHash hash =
    string "rootHash" (hash |> Hash.toUnprefixedString)


relativeTo : FQN -> QueryParameter
relativeTo fqn =
    string "relativeTo" (fqn |> FQN.toString)
