module UnisonShare.Session exposing
    ( Session(..)
    , account
    , decode
    , handle
    , hasProjectAccess
    , isHandle
    , isOrganizationMember
    , isProjectOwner
    , isSignedIn
    , isUnisonMember
    )

import Json.Decode as Decode
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UnisonShare.Account as Account exposing (AccountSummary)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type Session
    = Anonymous
    | SignedIn AccountSummary


isHandle : UserHandle -> Session -> Bool
isHandle handle_ session =
    case session of
        Anonymous ->
            False

        SignedIn a ->
            UserHandle.equals a.handle handle_


isSignedIn : Session -> Bool
isSignedIn session =
    case session of
        Anonymous ->
            False

        SignedIn _ ->
            True


isProjectOwner : ProjectRef -> Session -> Bool
isProjectOwner projectRef session =
    isHandle (ProjectRef.handle projectRef) session


hasProjectAccess : ProjectRef -> Session -> Bool
hasProjectAccess projectRef session =
    case session of
        Anonymous ->
            False

        SignedIn a ->
            let
                projectHandle =
                    ProjectRef.handle projectRef
            in
            UserHandle.equals projectHandle a.handle || Account.isOrganizationMember projectHandle a


isOrganizationMember : UserHandle -> Session -> Bool
isOrganizationMember orgHandle session =
    case session of
        Anonymous ->
            False

        SignedIn a ->
            Account.isOrganizationMember orgHandle a


isUnisonMember : Session -> Bool
isUnisonMember session =
    case session of
        Anonymous ->
            False

        SignedIn a ->
            Account.isUnisonMember a


handle : Session -> Maybe UserHandle
handle session =
    case session of
        Anonymous ->
            Nothing

        SignedIn a ->
            Just a.handle


account : Session -> Maybe AccountSummary
account session =
    case session of
        Anonymous ->
            Nothing

        SignedIn a ->
            Just a


decode : Decode.Decoder Session
decode =
    Decode.oneOf
        [ Decode.map SignedIn Account.decodeSummary
        , Decode.succeed Anonymous
        ]
