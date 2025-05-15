module UnisonShare.Page.OrgProfilePage exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi
import Lib.UserHandle exposing (UserHandle)
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import Set
import UI
import UI.Card as Card
import UI.Divider as Divider
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.ProfileSnippet as ProfileSnippet
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.Org exposing (OrgDetails)
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectSummary)
import UnisonShare.Project.ProjectListing as ProjectListing



-- MODEL


type alias ProfileFormFields =
    { bio : String }


type ProfileForm
    = Editing ProfileFormFields
    | Saving ProfileFormFields
      -- Success isn't tracked, we just close the modal.
    | SaveFailed ProfileFormFields Http.Error


type UserProfileModal
    = NoModal
    | EditProfileModal ProfileForm


type alias Model =
    { projects : WebData (List ProjectSummary)
    , modal : UserProfileModal
    }


init : AppContext -> UserHandle -> ( Model, Cmd Msg )
init appContext handle =
    ( { projects = Loading, modal = NoModal }
    , fetchProjects appContext handle
    )



-- UPDATE


type Msg
    = NoOp
    | FetchProjectsFinished (WebData (List ProjectSummary))
    | CloseModal


update : AppContext -> UserHandle -> OrgDetails -> Msg -> Model -> ( Model, Cmd Msg )
update _ _ _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FetchProjectsFinished projects ->
            ( { model | projects = projects }, Cmd.none )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none )



-- EFFECTS


fetchProjects : AppContext -> UserHandle -> Cmd Msg
fetchProjects appContext handle =
    ShareApi.userProjects handle
        |> HttpApi.toRequest
            (Decode.list Project.decodeSummary)
            (RemoteData.fromResult >> FetchProjectsFinished)
        |> HttpApi.perform appContext.api



-- VIEW


viewProjects : List ProjectSummary -> Html msg
viewProjects projects_ =
    case projects_ of
        [] ->
            UI.nothing

        projects ->
            let
                viewProject p =
                    let
                        tags =
                            case Set.toList p.tags of
                                [] ->
                                    UI.nothing

                                ts ->
                                    ts |> List.map Tag.tag |> Tag.viewTags
                    in
                    div [ class "project" ]
                        [ p
                            |> ProjectListing.projectListing
                            |> ProjectListing.withClick Link.userProfile Link.projectOverview
                            |> ProjectListing.view
                        , MaybeE.unwrap UI.nothing (\s -> div [ class "project-summary" ] [ text s ]) p.summary
                        , tags
                        ]

                card_ =
                    Card.titled
                        "Projects"
                        [ div [ class "projects" ]
                            (List.intersperse (Divider.divider |> Divider.small |> Divider.view)
                                (List.map viewProject projects)
                            )
                        ]
                        |> Card.asContained
            in
            Card.view card_


view_ :
    OrgDetails
    -> WebData (List ProjectSummary)
    -> PageContent Msg
view_ org projects =
    let
        pageContent =
            [ projects
                |> RemoteData.map viewProjects
                |> RemoteData.withDefault UI.nothing
            ]

        pageTitle =
            PageTitle.custom
                [ ProfileSnippet.profileSnippet
                    org
                    |> ProfileSnippet.huge
                    |> ProfileSnippet.view
                ]
    in
    PageContent.oneColumn pageContent
        |> PageContent.withPageTitle pageTitle


viewLoadingPage : PageLayout.PageLayout msg
viewLoadingPage =
    let
        content =
            PageContent.oneColumn
                [ div [ class "org-profile-page_page-content" ]
                    [ div [ class "org-profile_main-content" ]
                        [ text "" ]
                    ]
                ]
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


view : OrgDetails -> Model -> PageLayout.PageLayout Msg
view org model =
    PageLayout.centeredNarrowLayout
        (view_ org model.projects)
        PageFooter.pageFooter
        |> PageLayout.withSubduedBackground
