module UnisonShare.Page.UserProfilePage exposing (..)

import Html exposing (Html, div, form, text)
import Html.Attributes exposing (class)
import Http
import Json.Decode as Decode
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle exposing (UserHandle)
import Lib.Util exposing (unicodeStringLength)
import Markdown
import Maybe.Extra as MaybeE
import RemoteData exposing (RemoteData(..), WebData)
import Set
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Divider as Divider
import UI.Form.TextField as TextField
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent exposing (PageContent)
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.ProfileSnippet as ProfileSnippet
import UI.StatusBanner as StatusBanner
import UI.Tag as Tag
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter
import UnisonShare.Project as Project exposing (ProjectSummary)
import UnisonShare.Project.ProjectListing as ProjectListing
import UnisonShare.Session as Session exposing (Session)
import UnisonShare.User exposing (UserDetails)



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
    | ShowEditProfileModal
    | UpdateBioField String
    | SaveProfile
    | SaveProfileFinished (HttpResult ())
    | CloseModal


type OutMsg
    = NoOut
    | UpdateUserProfile UserDetails


update : AppContext -> UserHandle -> WebData UserDetails -> Msg -> Model -> ( Model, Cmd Msg, OutMsg )
update appContext handle user msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoOut )

        FetchProjectsFinished projects ->
            ( { model | projects = projects }, Cmd.none, NoOut )

        ShowEditProfileModal ->
            case ( Session.isHandle handle appContext.session, RemoteData.map .bio user ) of
                ( True, Success (Just b) ) ->
                    ( { model | modal = EditProfileModal (Editing { bio = b }) }, Cmd.none, NoOut )

                ( True, Success Nothing ) ->
                    ( { model | modal = EditProfileModal (Editing { bio = "" }) }, Cmd.none, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

        UpdateBioField b ->
            ( { model | modal = EditProfileModal (Editing { bio = b }) }, Cmd.none, NoOut )

        SaveProfile ->
            let
                form =
                    case model.modal of
                        EditProfileModal (Editing f) ->
                            Just f

                        EditProfileModal (SaveFailed f _) ->
                            Just f

                        _ ->
                            Nothing
            in
            case ( Session.handle appContext.session, form ) of
                ( Just h, Just f ) ->
                    ( { model | modal = EditProfileModal (Saving f) }, updateProfile appContext h f, NoOut )

                _ ->
                    ( model, Cmd.none, NoOut )

        SaveProfileFinished result ->
            case ( user, model.modal, result ) of
                ( Success u, EditProfileModal (Saving f), Ok _ ) ->
                    ( { model | modal = NoModal }
                    , Cmd.none
                    , UpdateUserProfile { u | bio = Just f.bio }
                    )

                ( _, EditProfileModal (Saving f), Err e ) ->
                    ( { model | modal = EditProfileModal (SaveFailed f e) }
                    , Cmd.none
                    , NoOut
                    )

                _ ->
                    ( model, Cmd.none, NoOut )

        CloseModal ->
            ( { model | modal = NoModal }, Cmd.none, NoOut )



-- EFFECTS


fetchProjects : AppContext -> UserHandle -> Cmd Msg
fetchProjects appContext handle =
    ShareApi.userProjects handle
        |> HttpApi.toRequest
            (Decode.list Project.decodeSummary)
            (RemoteData.fromResult >> FetchProjectsFinished)
        |> HttpApi.perform appContext.api


updateProfile : AppContext -> UserHandle -> ProfileFormFields -> Cmd Msg
updateProfile appContext handle fields =
    ShareApi.updateUserProfile handle fields
        |> HttpApi.toRequestWithEmptyResponse SaveProfileFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewEditProfileModal : ProfileForm -> Html Msg
viewEditProfileModal profileForm =
    let
        form_ f =
            form [ class "description-form" ]
                [ TextField.field UpdateBioField "Bio" f.bio
                    |> TextField.withHelpText (String.fromInt (unicodeStringLength f.bio) ++ "/400 characters.")
                    |> TextField.withRows 7
                    |> TextField.withMaxlength 400
                    |> TextField.withAutofocus
                    |> TextField.view
                ]

        content_ form__ =
            div
                [ class "edit-user-profile-modal_content" ]
                [ form__
                ]

        { content, showActions, dimOverlay, banner } =
            case profileForm of
                Editing f ->
                    { content = content_ (form_ f)
                    , showActions = True
                    , dimOverlay = False
                    , banner = UI.nothing
                    }

                Saving f ->
                    { content = content_ (form_ f)
                    , showActions = True
                    , dimOverlay = True
                    , banner = StatusBanner.working "Saving.."
                    }

                SaveFailed f _ ->
                    { content = content_ (form_ f)
                    , showActions = True
                    , dimOverlay = False
                    , banner = StatusBanner.bad "Save failed, try again"
                    }
    in
    Modal.modal "edit-user-profile-modal" CloseModal (Modal.Content content)
        |> Modal.withActionsIf
            [ Button.button CloseModal "Cancel"
                |> Button.subdued
                |> Button.medium
            , Button.button SaveProfile "Save"
                |> Button.emphasized
                |> Button.medium
            ]
            showActions
        |> Modal.withLeftSideFooter [ banner ]
        |> Modal.withDimOverlay dimOverlay
        |> Modal.withHeaderIf "Edit profile" showActions
        |> Modal.view


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
    Session
    -> UserDetails
    -> WebData (List ProjectSummary)
    -> PageContent Msg
view_ session user projects =
    let
        pageContent =
            [ MaybeE.unwrap
                UI.nothing
                (\b ->
                    Card.card [ Markdown.toHtml [] b ]
                        |> Card.asContained
                        |> Card.withTitle "BIO"
                        |> Card.withClassName "bio"
                        |> Card.view
                )
                user.bio
            , projects
                |> RemoteData.map viewProjects
                |> RemoteData.withDefault UI.nothing
            ]

        isViewingOwnProfile =
            Session.isHandle user.handle session

        titleRightSide =
            if isViewingOwnProfile then
                [ div [ class "user-profile-empty-state_instructions-banner" ]
                    [ text (UserHandle.toString user.handle ++ " is you! 😎 This is your profile—your space!")
                    , Button.iconThenLabel ShowEditProfileModal Icon.writingPad "Edit profile"
                        |> Button.small
                        |> Button.view
                    ]
                ]

            else
                []

        pageTitle =
            PageTitle.custom
                [ ProfileSnippet.profileSnippet
                    user
                    |> ProfileSnippet.huge
                    |> ProfileSnippet.view
                ]
                |> PageTitle.withRightSide titleRightSide
    in
    PageContent.oneColumn pageContent
        |> PageContent.withPageTitle pageTitle


viewLoadingPage : PageLayout.PageLayout msg
viewLoadingPage =
    let
        content =
            PageContent.oneColumn
                [ div [ class "user-profile-page_page-content" ]
                    [ div [ class "user-profile_main-content" ]
                        [ text "" ]
                    ]
                ]
    in
    PageLayout.centeredNarrowLayout content PageFooter.pageFooter
        |> PageLayout.withSubduedBackground


view : Session -> UserDetails -> Model -> ( PageLayout.PageLayout Msg, Maybe (Html Msg) )
view session user model =
    let
        page =
            PageLayout.centeredNarrowLayout
                (view_ session user model.projects)
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground

        modal =
            case model.modal of
                NoModal ->
                    Nothing

                EditProfileModal form ->
                    Just (viewEditProfileModal form)
    in
    ( page, modal )
