module UnisonShare.Page.AccountPage exposing
    ( Model(..)
    , Msg(..)
    , init
    , update
    , view
    )

import Html exposing (Html, div, em, p, section, strong, text)
import Html.Attributes exposing (class)
import Lib.HttpApi as HttpApi exposing (HttpResult)
import Lib.UserHandle as UserHandle
import RemoteData exposing (RemoteData(..), WebData)
import UI
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Icon as Icon
import UI.Modal as Modal
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.PageTitle as PageTitle
import UI.StatusBanner as StatusBanner
import UnisonShare.Account exposing (Account)
import UnisonShare.Api as ShareApi
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppHeader as AppHeader
import UnisonShare.PageFooter as PageFooter



-- MODEL


type Confirm
    = NotConfirmed
    | Confirmed (WebData ())


type Model
    = NoModal
    | ExportDataModal Confirm
    | DeleteAccountModal Confirm


init : Model
init =
    NoModal



-- MSG


type Msg
    = ShowExportDataModal
    | ExportDataConfirm
    | ExportDataRequestFinished (HttpResult ())
    | ShowDeleteAccountModal
    | DeleteAccountConfirm
    | DeleteAccountRequestFinished (HttpResult ())
    | CloseModal


update : AppContext -> Account a -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ msg _ =
    case msg of
        ShowExportDataModal ->
            ( ExportDataModal NotConfirmed, Cmd.none )

        ExportDataConfirm ->
            ( ExportDataModal (Confirmed Loading), exportDataRequest appContext )

        ExportDataRequestFinished (Ok r) ->
            ( ExportDataModal (Confirmed (Success r)), Cmd.none )

        ExportDataRequestFinished (Err e) ->
            ( ExportDataModal (Confirmed (Failure e)), Cmd.none )

        ShowDeleteAccountModal ->
            ( DeleteAccountModal NotConfirmed, Cmd.none )

        DeleteAccountConfirm ->
            ( DeleteAccountModal (Confirmed Loading), deleteAccountRequest appContext )

        DeleteAccountRequestFinished (Ok r) ->
            ( DeleteAccountModal (Confirmed (Success r)), Cmd.none )

        DeleteAccountRequestFinished (Err e) ->
            ( DeleteAccountModal (Confirmed (Failure e)), Cmd.none )

        CloseModal ->
            ( NoModal, Cmd.none )



-- EFFECTS


exportDataRequest : AppContext -> Cmd Msg
exportDataRequest appContext =
    let
        data =
            { subject = "Export my Unison Share data"
            , body = "Automated support request to export my Unison Share data"
            , tags = [ "export-data" ]
            }
    in
    ShareApi.createSupportTicket data
        |> HttpApi.toRequestWithEmptyResponse ExportDataRequestFinished
        |> HttpApi.perform appContext.api


deleteAccountRequest : AppContext -> Cmd Msg
deleteAccountRequest appContext =
    let
        data =
            { subject = "Delete my Unison Share account"
            , body = "Automated support request to delete my Unison Share account"
            , tags = [ "delete-account" ]
            }
    in
    ShareApi.createSupportTicket data
        |> HttpApi.toRequestWithEmptyResponse ExportDataRequestFinished
        |> HttpApi.perform appContext.api



-- VIEW


viewStatus : WebData () -> Html Msg
viewStatus status =
    let
        loading =
            div [ class "status" ]
                [ StatusBanner.working "Creating a support ticket for your request..."
                ]
    in
    case status of
        NotAsked ->
            loading

        Loading ->
            loading

        Success _ ->
            div [ class "status" ]
                [ StatusBanner.good "We've created a support ticket for your request. We'll send an update as soon as possible."
                , Button.iconThenLabel CloseModal Icon.thumbsUp "Got it!" |> Button.medium |> Button.emphasized |> Button.view
                ]

        Failure _ ->
            div [ class "status" ]
                [ StatusBanner.bad "Something didn't quite work in trying to create your request. Please try again. We've also been notified about this issue."
                , Button.iconThenLabel CloseModal Icon.thumbsUp "Close" |> Button.medium |> Button.emphasized |> Button.view
                ]


viewExportDataModal : Confirm -> Html Msg
viewExportDataModal confirm =
    let
        viewContent action =
            Modal.Content
                (section [ class "info-modal-content" ]
                    [ p [] [ text "We don't yet have an automated export system, and are handling requests via our support system." ]
                    , action
                    ]
                )

        content =
            case confirm of
                NotConfirmed ->
                    viewContent (Button.button ExportDataConfirm "Create support ticket" |> Button.emphasized |> Button.view)

                Confirmed status ->
                    viewContent (viewStatus status)
    in
    Modal.modal "info-modal" CloseModal content
        |> Modal.withHeader "Export Data"
        |> Modal.view


viewDeleteAccountModal : Account a -> Confirm -> Html Msg
viewDeleteAccountModal a confirm =
    let
        viewContent action =
            Modal.Content
                (section [ class "info-modal-content" ]
                    [ p []
                        [ text "We're "
                        , em [] [ text "really" ]
                        , text " sorry to hear you're looking to delete your "
                        , strong [] [ text (UserHandle.toString a.handle) ]
                        , text " account."
                        ]
                    , p [] [ text "We don't yet have an automatic deletion system in place, and are handling it via our support system." ]
                    , action
                    ]
                )

        content =
            case confirm of
                NotConfirmed ->
                    viewContent (Button.button DeleteAccountConfirm "Create support ticket" |> Button.emphasized |> Button.view)

                Confirmed status ->
                    viewContent (viewStatus status)
    in
    Modal.modal "info-modal" CloseModal content
        |> Modal.withHeader "Delete Account"
        |> Modal.view


view : Account a -> Model -> AppDocument Msg
view account model =
    let
        content =
            [ Card.titled "Account"
                [ Button.iconThenLabel_ (Click.onClick ShowExportDataModal) Icon.download "Export data" |> Button.view
                , div [] [ text "Download a zip of all your data on Unison Share. Including your codebase and user settings." ]
                , UI.divider
                , Button.iconThenLabel_ (Click.onClick ShowDeleteAccountModal) Icon.warn "Delete Account" |> Button.critical |> Button.view
                , p [] [ text "Deleting your account will remove all data associated with your account. References to your authors will remain in place, but no longer tied to an account." ]
                , p [] [ text "Deletion of your account can’t be completed if you own any published projects—ownership must be transfered." ]
                ]
                |> Card.asContained
                |> Card.view
            ]

        page =
            PageLayout.centeredNarrowLayout
                (PageContent.oneColumn content
                    |> PageContent.withPageTitle
                        (PageTitle.title "Account Settings"
                            |> PageTitle.withIcon Icon.cog
                            |> PageTitle.withDescription "Manage your account settings"
                        )
                )
                PageFooter.pageFooter
                |> PageLayout.withSubduedBackground

        modal =
            case model of
                ExportDataModal confirm ->
                    Just (viewExportDataModal confirm)

                DeleteAccountModal confirm ->
                    Just (viewDeleteAccountModal account confirm)

                _ ->
                    Nothing
    in
    { pageId = "account"
    , title = "Account"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = modal
    }
