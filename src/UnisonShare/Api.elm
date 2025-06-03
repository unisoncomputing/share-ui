module UnisonShare.Api exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import Code.CodebaseApi as CodebaseApi
import Code.Definition.Reference as Reference
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
import UnisonShare.Account exposing (Account)
import UnisonShare.CodeBrowsingContext exposing (CodeBrowsingContext(..))
import UnisonShare.Contribution exposing (ContributionStateToken(..))
import UnisonShare.Contribution.ContributionRef as ContributionRef exposing (ContributionRef)
import UnisonShare.Contribution.ContributionStatus as ContributionStatus exposing (ContributionStatus)
import UnisonShare.DefinitionDiff as DefinitionDiff
import UnisonShare.Notification as Notification exposing (NotificationStatus)
import UnisonShare.OrgMember as OrgMember exposing (OrgMember)
import UnisonShare.OrgRole as OrgRole
import UnisonShare.Paginated as Paginated exposing (PageCursorParam)
import UnisonShare.Project as Project exposing (ProjectVisibility)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.ProjectCollaborator exposing (ProjectCollaborator)
import UnisonShare.ProjectRole as ProjectRole
import UnisonShare.Ticket.TicketRef as TicketRef exposing (TicketRef)
import UnisonShare.Ticket.TicketStatus as TicketStatus exposing (TicketStatus)
import UnisonShare.Timeline.CommentId as CommentId exposing (CommentId)
import UnisonShare.Tour as Tour exposing (Tour)
import Url.Builder exposing (QueryParameter, int, string)


profile : UserHandle -> Endpoint
profile handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle ], queryParams = [] }


user : UserHandle -> Endpoint
user handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle ], queryParams = [] }


org : UserHandle -> Endpoint
org handle =
    GET { path = [ "users", UserHandle.toUnprefixedString handle ], queryParams = [] }


updateUserProfile : UserHandle -> { bio : String } -> Endpoint
updateUserProfile handle profile_ =
    let
        body =
            Encode.object [ ( "bio", Encode.string profile_.bio ) ]
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
    , cursor : PageCursorParam
    }


userBranches : UserHandle -> UserBranchesParams -> Endpoint
userBranches handle params =
    let
        queryParams =
            int "limit" params.limit
                :: Paginated.toQueryParams params.cursor
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


searchNames : List ( String, String ) -> String -> Endpoint
searchNames params query =
    let
        params_ =
            params
                |> List.map (\( k, v ) -> string k v)
    in
    GET
        { path = [ "search-names" ]
        , queryParams = string "query" query :: params_
        }


searchDefinitions : Maybe ( String, String ) -> String -> Endpoint
searchDefinitions filter query =
    let
        filter_ =
            filter
                |> Maybe.map (\( k, v ) -> [ string k v ])
                |> Maybe.withDefault []
    in
    GET
        { path = [ "search-definitions" ]
        , queryParams = string "query" query :: filter_
        }


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



-- NOTIFICATIONS


notifications : Account a -> Maybe NotificationStatus -> PageCursorParam -> Endpoint
notifications account status paginationCursor =
    let
        statusQueryParams =
            case status of
                Just s ->
                    [ string "status" (Notification.statusToString s) ]

                Nothing ->
                    []

        paginationQueryParams =
            Paginated.toQueryParams paginationCursor
    in
    GET
        { path =
            [ "users"
            , UserHandle.toUnprefixedString account.handle
            , "notifications"
            , "hub"
            ]
        , queryParams = statusQueryParams ++ paginationQueryParams
        }



-- ORGS
-- ORG ROLE ASSIGNMENTS (COLLABORATORS)


createOrg : { a | handle : UserHandle } -> String -> UserHandle -> Bool -> Endpoint
createOrg owner name orgHandle isCommercial =
    let
        body =
            Encode.object
                [ ( "name", Encode.string name )
                , ( "handle", Encode.string (UserHandle.toUnprefixedString orgHandle) )
                , ( "isCommercial", Encode.bool isCommercial )
                , ( "owner", Encode.string (UserHandle.toUnprefixedString owner.handle) )
                ]
    in
    POST
        { path = [ "orgs" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


orgRoleAssignments : UserHandle -> Endpoint
orgRoleAssignments orgHandle =
    let
        handle =
            UserHandle.toUnprefixedString orgHandle
    in
    GET
        { path = [ "orgs", handle, "roles" ]
        , queryParams = []
        }


createOrgRoleAssignment : UserHandle -> List OrgMember -> Endpoint
createOrgRoleAssignment orgHandle members =
    let
        handle =
            UserHandle.toUnprefixedString orgHandle

        toAssignment member =
            case member of
                OrgMember.UserMember u ->
                    Encode.object
                        [ ( "subject"
                          , Encode.object
                                [ ( "kind", Encode.string "user" )
                                , ( "id", Encode.string u.user.id )
                                ]
                          )
                        , ( "roles", Encode.list OrgRole.encode u.roles )
                        ]

                OrgMember.TeamMember t ->
                    Encode.object
                        [ ( "subject"
                          , Encode.object
                                [ ( "kind", Encode.string "team" )
                                , ( "id", Encode.string t.teamId )
                                ]
                          )
                        , ( "roles", Encode.list OrgRole.encode t.roles )
                        ]

        body =
            Encode.object
                [ ( "role_assignments", Encode.list toAssignment members ) ]
    in
    POST
        { path = [ "orgs", handle, "roles" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


deleteOrgRoleAssignment : UserHandle -> OrgMember -> Endpoint
deleteOrgRoleAssignment orgHandle member =
    let
        handle =
            UserHandle.toUnprefixedString orgHandle

        toAssignment member_ =
            case member_ of
                OrgMember.UserMember u ->
                    Encode.object
                        [ ( "subject"
                          , Encode.object
                                [ ( "kind", Encode.string "user" )
                                , ( "id", Encode.string u.user.id )
                                ]
                          )
                        , ( "roles", Encode.list OrgRole.encode u.roles )
                        ]

                OrgMember.TeamMember t ->
                    Encode.object
                        [ ( "subject"
                          , Encode.object
                                [ ( "kind", Encode.string "team" )
                                , ( "id", Encode.string t.teamId )
                                ]
                          )
                        , ( "roles", Encode.list OrgRole.encode t.roles )
                        ]

        body =
            Encode.object
                [ ( "role_assignments", Encode.list toAssignment [ member ] ) ]
    in
    DELETE
        { path = [ "orgs", handle, "roles" ]
        , queryParams = []
        , body = Http.jsonBody body
        }



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
    , cursor : PageCursorParam
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
                ++ Paginated.toQueryParams params.cursor
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



-- PROJECT ROLE ASSIGNMENTS (COLLABORATORS)


projectRoleAssignments : ProjectRef -> Endpoint
projectRoleAssignments projectRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "roles" ]
        , queryParams = []
        }


createProjectRoleAssignment : ProjectRef -> List ProjectCollaborator -> Endpoint
createProjectRoleAssignment projectRef collaborators =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        toAssignment collab =
            Encode.object
                [ ( "subject"
                  , Encode.object
                        [ ( "kind", Encode.string "user" )
                        , ( "id", Encode.string collab.user.id )
                        ]
                  )
                , ( "roles", Encode.list ProjectRole.encode collab.roles )
                ]

        body =
            Encode.object
                [ ( "role_assignments", Encode.list toAssignment collaborators ) ]
    in
    POST
        { path = [ "users", handle, "projects", slug, "roles" ]
        , queryParams = []
        , body = Http.jsonBody body
        }


deleteProjectRoleAssignment : ProjectRef -> ProjectCollaborator -> Endpoint
deleteProjectRoleAssignment projectRef collaborator =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        toAssignment collab =
            Encode.object
                [ ( "subject"
                  , Encode.object
                        [ ( "kind", Encode.string "user" )
                        , ( "id", Encode.string collab.user.id )
                        ]
                  )
                , ( "roles", Encode.list ProjectRole.encode collab.roles )
                ]

        body =
            Encode.object
                [ ( "role_assignments", Encode.list toAssignment [ collaborator ] ) ]
    in
    DELETE
        { path = [ "users", handle, "projects", slug, "roles" ]
        , queryParams = []
        , body = Http.jsonBody body
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


projectContributionCheckMergeability : ProjectRef -> ContributionRef -> Endpoint
projectContributionCheckMergeability projectRef contribRef =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef, "merge", "check" ]
        , queryParams = []
        }


projectContributionMerge : ProjectRef -> ContributionRef -> ContributionStateToken -> Endpoint
projectContributionMerge projectRef contribRef (ContributionStateToken token) =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        body =
            Encode.object
                [ ( "contributionStateToken", Encode.string token )
                ]
    in
    POST
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef, "merge" ]
        , queryParams = []
        , body = Http.jsonBody body
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
        , body = Http.emptyBody
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


projectContributions : ProjectRef -> ContributionStatus -> Endpoint
projectContributions projectRef contributionStatus =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions" ]
        , queryParams = [ string "status" (ContributionStatus.toApiString contributionStatus) ]
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


projectContributionDefinitionDiff : ProjectRef -> ContributionRef -> DefinitionDiff.DefinitionType -> { old : FQN, new : FQN } -> Endpoint
projectContributionDefinitionDiff projectRef contribRef defType { old, new } =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        ( queryParams, endPoint ) =
            case defType of
                DefinitionDiff.Term ->
                    ( [ string "oldTerm" (FQN.toApiUrlString old)
                      , string "newTerm" (FQN.toApiUrlString new)
                      ]
                    , "terms"
                    )

                DefinitionDiff.Type ->
                    ( [ string "oldType" (FQN.toApiUrlString old)
                      , string "newType" (FQN.toApiUrlString new)
                      ]
                    , "types"
                    )
    in
    GET
        { path = [ "users", handle, "projects", slug, "contributions", ContributionRef.toApiString contribRef, "diff", endPoint ]
        , queryParams = queryParams
        }


type DefinitionDiffParams
    = Term { oldBranchRef : BranchRef, newBranchRef : BranchRef, oldTerm : FQN, newTerm : FQN }
    | Type { oldBranchRef : BranchRef, newBranchRef : BranchRef, oldType : FQN, newType : FQN }


projectBranchDefinitionDiff : ProjectRef -> DefinitionDiffParams -> Endpoint
projectBranchDefinitionDiff projectRef params =
    let
        ( handle, slug ) =
            ProjectRef.toApiStringParts projectRef

        ( queryParams, endPoint ) =
            case params of
                Term { oldBranchRef, newBranchRef, oldTerm, newTerm } ->
                    ( [ string "oldBranchRef" (BranchRef.toString oldBranchRef)
                      , string "newBranchRef" (BranchRef.toString newBranchRef)
                      , string "oldTerm" (FQN.toApiUrlString oldTerm)
                      , string "newTerm" (FQN.toApiUrlString newTerm)
                      ]
                    , "terms"
                    )

                Type { oldBranchRef, newBranchRef, oldType, newType } ->
                    ( [ string "oldBranchRef" (BranchRef.toString oldBranchRef)
                      , string "newBranchRef" (BranchRef.toApiUrlString newBranchRef)
                      , string "oldType" (FQN.toApiUrlString oldType)
                      , string "newType" (FQN.toApiUrlString newType)
                      ]
                    , "types"
                    )
    in
    GET
        { path = [ "users", handle, "projects", slug, "diff", endPoint ]
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


projectBranchDefinitionByName : ProjectRef -> BranchRef -> FQN -> Endpoint
projectBranchDefinitionByName projectRef branchRef fqn =
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
            , "definitions"
            , "by-name"
            , FQN.toApiUrlString fqn
            ]
        , queryParams = []
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
        , body = Http.emptyBody
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
        , body = Http.emptyBody
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
        , body = Http.emptyBody
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
                -- July 16 2024. SH: I wish the above comment said why it was disabled...
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
