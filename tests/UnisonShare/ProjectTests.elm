module UnisonShare.ProjectTests exposing (..)

import Code.ProjectSlug as ProjectSlug
import Expect
import Lib.UserHandle as UserHandle
import Set
import Test exposing (..)
import Time
import UI.DateTime as DateTime
import UnisonShare.Project as Project exposing (Project)
import UnisonShare.Project.ProjectRef as ProjectRef
import UnisonShare.Project.ReleaseDownloads exposing (ReleaseDownloads(..))


ref : Test
ref =
    describe "Project.ref"
        [ test "Returns the ref of a project by handle and slug" <|
            \_ ->
                Expect.equal
                    "@unison/http"
                    (Project.ref project |> ProjectRef.toString)
        ]


handle : Test
handle =
    describe "Project.handle"
        [ test "Returns the handle of a project" <|
            \_ ->
                Expect.equal
                    "@unison"
                    (Project.handle project |> UserHandle.toString)
        ]


slug : Test
slug =
    describe "Project.slug"
        [ test "Returns the slug of a project" <|
            \_ ->
                Expect.equal
                    "http"
                    (Project.slug project |> ProjectSlug.toString)
        ]


toggleFav : Test
toggleFav =
    let
        resultFor isFaved =
            let
                p =
                    Project.toggleFav (projectDetailsForFav isFaved 3)
            in
            ( p.isFaved, p.numFavs )
    in
    describe "Project.toggleFav"
        [ test "toggles FavUnknown to FavUnknown" <|
            \_ ->
                Expect.equal (resultFor Project.FavUnknown) ( Project.FavUnknown, 3 )
        , test "toggles Faved to NotFaved" <|
            \_ ->
                Expect.equal (resultFor Project.Faved) ( Project.NotFaved, 2 )
        , test "toggles JustFaved to NotFaved" <|
            \_ ->
                Expect.equal (resultFor Project.JustFaved) ( Project.NotFaved, 2 )
        , test "toggles NotFaved to JustFaved" <|
            \_ ->
                Expect.equal (resultFor Project.NotFaved) ( Project.JustFaved, 4 )
        ]


toggleSubscription : Test
toggleSubscription =
    let
        resultFor isSubscribed =
            let
                p =
                    Project.toggleSubscription (projectDetailsForSub isSubscribed)
            in
            p.isSubscribed
    in
    describe "Project.toggleSubscription"
        [ test "toggles SubUnknown to SubUnknown" <|
            \_ ->
                Expect.equal (resultFor Project.SubUnknown) Project.SubUnknown
        , test "toggles Subscribed to NotSubscribed" <|
            \_ ->
                Expect.equal (resultFor Project.Subscribed) Project.NotSubscribed
        , test "toggles JustSubscribed to NotSubscribed" <|
            \_ ->
                Expect.equal (resultFor Project.JustSubscribed) Project.NotSubscribed
        , test "toggles NotSubscribed to JustSubscribed" <|
            \_ ->
                Expect.equal (resultFor Project.NotSubscribed) Project.JustSubscribed
        ]



-- Helpers


project : Project {}
project =
    { ref =
        ProjectRef.projectRef
            (UserHandle.unsafeFromString "unison")
            (ProjectSlug.unsafeFromString "http")
    , visibility = Project.Public
    }


projectDetailsForFav : Project.IsFaved -> Int -> Project.ProjectDetails
projectDetailsForFav isFaved numFavs =
    { ref =
        ProjectRef.projectRef
            (UserHandle.unsafeFromString "unison")
            (ProjectSlug.unsafeFromString "http")
    , isFaved = isFaved
    , isSubscribed = Project.NotSubscribed
    , numFavs = numFavs
    , numActiveContributions = 0
    , numOpenTickets = 0
    , releaseDownloads = ReleaseDownloads []
    , summary = Just "hi i'm a summary"
    , tags = Set.empty
    , visibility = Project.Public
    , latestVersion = Nothing
    , defaultBranch = Nothing
    , permissions = []
    , createdAt = DateTime.fromPosix (Time.millisToPosix 1)
    , updatedAt = DateTime.fromPosix (Time.millisToPosix 1)
    , isPremiumProject = False
    }


projectDetailsForSub : Project.IsSubscribed -> Project.ProjectDetails
projectDetailsForSub isSubscribed =
    { ref =
        ProjectRef.projectRef
            (UserHandle.unsafeFromString "unison")
            (ProjectSlug.unsafeFromString "http")
    , isFaved = Project.NotFaved
    , isSubscribed = isSubscribed
    , numFavs = 42
    , numActiveContributions = 0
    , numOpenTickets = 0
    , releaseDownloads = ReleaseDownloads []
    , summary = Just "hi i'm a summary"
    , tags = Set.empty
    , visibility = Project.Public
    , latestVersion = Nothing
    , defaultBranch = Nothing
    , permissions = []
    , isPremiumProject = True
    , createdAt = DateTime.fromPosix (Time.millisToPosix 1)
    , updatedAt = DateTime.fromPosix (Time.millisToPosix 1)
    }
