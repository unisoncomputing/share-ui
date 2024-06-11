module UnisonShare.Page.AppErrorPage exposing (..)

import Html exposing (br, p, text)
import Html.Attributes exposing (class)
import UI.Button as Button
import UI.Card as Card
import UI.Icon as Icon
import UI.PageContent as PageContent
import UI.PageLayout as PageLayout
import UI.StatusMessage as StatusMessage
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.AppDocument exposing (AppDocument)
import UnisonShare.AppError exposing (AppError(..))
import UnisonShare.AppHeader as AppHeader
import UnisonShare.Link as Link
import UnisonShare.PageFooter as PageFooter


view : AppContext -> AppError -> AppDocument msg
view appContext appError =
    let
        card =
            case appError of
                AccountCreationGitHubPermissionsRejected ->
                    StatusMessage.bad "Couldn't Create Account"
                        [ p []
                            [ text "It looks like you rejected the permission request when creating your account with GitHub."
                            , br [] []
                            , text "We need these core pieces of data from your GitHub in order to create your Unison Share account."
                            ]
                        , p [] [ text "Please try again." ]
                        ]
                        |> StatusMessage.withCta (Button.iconThenLabel_ (Link.login appContext.api appContext.currentUrl) Icon.github "Create Account with GitHub" |> Button.medium)
                        |> StatusMessage.asCard

                AccountCreationHandleAlreadyTaken ->
                    StatusMessage.bad "Couldn't Create Account"
                        [ p []
                            [ text "It looks like the selected handle already exists within Unison Share."
                            ]
                        , p [] [ text "Please try again with a different handle." ]
                        ]
                        |> StatusMessage.withCta (Button.iconThenLabel_ (Link.login appContext.api appContext.currentUrl) Icon.github "Create Account with GitHub" |> Button.medium)
                        |> StatusMessage.asCard

                UnspecifiedError ->
                    StatusMessage.bad "Something went wrong 😞"
                        [ p [] [ text "Unfortunately, we couldn't successfully complete your request." ]
                        , p [ class "subtle" ]
                            [ text "The Unison team have been notified about this error,"
                            , br [] []
                            , text " so that we can help prevent it in the future."
                            ]
                        ]
                        |> StatusMessage.withCta (Button.iconThenLabel_ Link.reportShareBug Icon.bug "Report a Share Bug" |> Button.medium)
                        |> StatusMessage.asCard

        page =
            PageLayout.centeredLayout
                (PageContent.oneColumn [ Card.view card ])
                PageFooter.pageFooter
    in
    { pageId = "error-page"
    , title = "Something went wrong 😞"
    , appHeader = AppHeader.appHeader AppHeader.None
    , pageHeader = Nothing
    , page = PageLayout.view page
    , modal = Nothing
    }
