module UnisonShare.Account exposing (..)

import Json.Decode as Decode exposing (string)
import Json.Decode.Pipeline exposing (optional, required)
import Lib.Decode.Helpers exposing (url)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import UI.Avatar as Avatar exposing (Avatar)
import UI.Icon as Icon
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)
import UnisonShare.Tour as Tour exposing (Tour)
import UnisonShare.UnisonPlan as UnisonPlan exposing (UnisonPlan)
import UnisonShare.User exposing (UserSummary)
import Url exposing (Url)


type alias Account a =
    { a
        | handle : UserHandle
        , name : Maybe String
        , avatarUrl : Maybe Url
        , pronouns : Maybe String
        , completedTours : List Tour
        , organizationMemberships : List OrganizationMembership
        , isSuperAdmin : Bool
        , primaryEmail : String
        , plan : UnisonPlan
        , hasUnreadNotifications : Bool
    }


type OrganizationMembership
    = OrganizationMembership UserHandle


type alias AccountSummary =
    Account {}



-- HELPERS


toUserSummary : Account a -> UserSummary
toUserSummary account =
    { handle = account.handle
    , name = account.name
    , avatarUrl = account.avatarUrl
    , pronouns = account.pronouns
    }


name : Account a -> String
name account =
    Maybe.withDefault (UserHandle.toString account.handle) account.name


isOrganizationMember : UserHandle -> Account a -> Bool
isOrganizationMember orgHandle account =
    account.organizationMemberships
        |> List.map (\(OrganizationMembership handle) -> handle)
        |> List.member orgHandle


isUnisonMember : Account a -> Bool
isUnisonMember account =
    isOrganizationMember (UserHandle.unsafeFromString "unison") account


toAvatar : Account a -> Avatar msg
toAvatar account =
    Avatar.avatar account.avatarUrl (Just (name account))
        |> Avatar.withIcon Icon.user


hasCompletedTour : Tour -> Account a -> Bool
hasCompletedTour tour { completedTours } =
    List.member tour completedTours


markTourAsCompleted : Tour -> Account a -> Account a
markTourAsCompleted tour account =
    { account | completedTours = account.completedTours ++ [ tour ] }


isProjectOwner : ProjectRef -> Account a -> Bool
isProjectOwner projectRef account =
    UserHandle.equals (ProjectRef.handle projectRef) account.handle



-- DECODE


decodeSummary : Decode.Decoder AccountSummary
decodeSummary =
    let
        makeSummary handle name_ avatarUrl completedTours organizationMemberships isSuperAdmin primaryEmail plan hasUnreadNotifications =
            { handle = handle
            , name = name_
            , avatarUrl = avatarUrl
            , pronouns = Nothing
            , completedTours = Maybe.withDefault [] completedTours
            , organizationMemberships = organizationMemberships
            , isSuperAdmin = isSuperAdmin
            , primaryEmail = primaryEmail
            , plan = plan
            , hasUnreadNotifications = hasUnreadNotifications
            }
    in
    Decode.succeed makeSummary
        |> required "handle" UserHandle.decodeUnprefixed
        |> optional "name" (Decode.map Just string) Nothing
        |> optional "avatarUrl" (Decode.map Just url) Nothing
        |> optional "completedTours" (Decode.map Just (Decode.list Tour.decode)) Nothing
        |> required "organizationMemberships" (Decode.list (Decode.map OrganizationMembership UserHandle.decodeUnprefixed))
        |> optional "isSuperadmin" Decode.bool False
        |> required "primaryEmail" string
        |> required "planTier" UnisonPlan.decode
        |> required "hasUnreadNotifications" Decode.bool
