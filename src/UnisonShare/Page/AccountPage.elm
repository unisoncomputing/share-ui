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
import UI.Button as Button
import UI.Card as Card
import UI.Click as Click
import UI.Divider as Divider
import UI.Form.TextField as TextField
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
    | ExportDataModal String Confirm
    | DeleteAccountModal String Confirm


init : Model
init =
    NoModal



-- MSG


type Msg
    = ShowExportDataModal
    | ExportDataConfirm
    | UpdateExportDataMessage String
    | ExportDataRequestFinished (HttpResult ())
    | ShowDeleteAccountModal
    | UpdateDeleteAccountMessage String
    | DeleteAccountConfirm
    | DeleteAccountRequestFinished (HttpResult ())
    | CloseModal


update : AppContext -> Account a -> Msg -> Model -> ( Model, Cmd Msg )
update appContext _ msg model =
    case msg of
        ShowExportDataModal ->
            ( ExportDataModal "" NotConfirmed, Cmd.none )

        UpdateExportDataMessage val ->
            case model of
                ExportDataModal _ confirm ->
                    ( ExportDataModal val confirm, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExportDataConfirm ->
            case model of
                ExportDataModal val _ ->
                    ( ExportDataModal val (Confirmed Loading), exportDataRequest appContext val )

                _ ->
                    ( model, Cmd.none )

        ExportDataRequestFinished (Ok r) ->
            case model of
                ExportDataModal val _ ->
                    ( ExportDataModal val (Confirmed (Success r)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExportDataRequestFinished (Err e) ->
            case model of
                ExportDataModal val _ ->
                    ( ExportDataModal val (Confirmed (Failure e)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowDeleteAccountModal ->
            ( DeleteAccountModal "" NotConfirmed, Cmd.none )

        UpdateDeleteAccountMessage val ->
            case model of
                DeleteAccountModal _ confirm ->
                    ( DeleteAccountModal val confirm, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DeleteAccountConfirm ->
            case model of
                DeleteAccountModal val _ ->
                    ( DeleteAccountModal val (Confirmed Loading), deleteAccountRequest appContext val )

                _ ->
                    ( model, Cmd.none )

        DeleteAccountRequestFinished (Ok r) ->
            case model of
                DeleteAccountModal val _ ->
                    ( DeleteAccountModal val (Confirmed (Success r)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        DeleteAccountRequestFinished (Err e) ->
            case model of
                DeleteAccountModal val _ ->
                    ( DeleteAccountModal val (Confirmed (Failure e)), Cmd.none )

                _ ->
                    ( model, Cmd.none )

        CloseModal ->
            ( NoModal, Cmd.none )



-- EFFECTS


exportDataRequest : AppContext -> String -> Cmd Msg
exportDataRequest appContext details =
    let
        body =
            if String.isEmpty details then
                "Automated support request to export my Unison Share data"

            else
                details

        data =
            { subject = "Export my Unison Share data"
            , body = body
            , tags = [ "export-data" ]
            }
    in
    ShareApi.createSupportTicket data
        |> HttpApi.toRequestWithEmptyResponse ExportDataRequestFinished
        |> HttpApi.perform appContext.api


deleteAccountRequest : AppContext -> String -> Cmd Msg
deleteAccountRequest appContext reason =
    let
        body =
            if String.isEmpty reason then
                "Automated support request to delete my Unison Share account"

            else
                reason

        data =
            { subject = "Delete my Unison Share account"
            , body = body
            , tags = [ "delete-account" ]
            }
    in
    ShareApi.createSupportTicket data
        |> HttpApi.toRequestWithEmptyResponse DeleteAccountRequestFinished
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


viewExportDataModal : String -> Confirm -> Html Msg
viewExportDataModal message confirm =
    let
        viewContent action =
            Modal.Content
                (section [ class "info-modal-content" ]
                    [ p [] [ text "We don't yet have an automated export system, and are handling requests via our support system." ]
                    , TextField.field UpdateExportDataMessage "What are you looking to export?" message
                        |> TextField.withRows 4
                        |> TextField.view
                    , action
                    ]
                )

        content =
            case confirm of
                NotConfirmed ->
                    viewContent (Button.button ExportDataConfirm "Submit support ticket" |> Button.emphasized |> Button.view)

                Confirmed status ->
                    viewContent (viewStatus status)
    in
    Modal.modal "info-modal" CloseModal content
        |> Modal.withHeader "Export Data"
        |> Modal.view


viewDeleteAccountModal : Account a -> String -> Confirm -> Html Msg
viewDeleteAccountModal a message confirm =
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
                    , div [] [ text "We don't yet have an automatic deletion system in place, and are handling it via our support system." ]
                    , Divider.divider |> Divider.small |> Divider.view
                    , TextField.field UpdateDeleteAccountMessage "Delete reason (optional)" message
                        |> TextField.withPlaceholder "Say a few works aboyt why you're looking to delete your account"
                        |> TextField.withRows 4
                        |> TextField.view
                    , action
                    ]
                )

        content =
            case confirm of
                NotConfirmed ->
                    viewContent (Button.button DeleteAccountConfirm "Submit support ticket" |> Button.emphasized |> Button.view)

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
                , Divider.divider |> Divider.small |> Divider.view
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
                ExportDataModal val confirm ->
                    Just (viewExportDataModal val confirm)

                DeleteAccountModal val confirm ->
                    Just (viewDeleteAccountModal account val confirm)

                _ ->
                    Nothing
    in
    { pageId = "account"
    , title = "Account"
    , appHeader = AppHeader.appHeader
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = modal
    }
