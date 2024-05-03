module UnisonShare.AppError exposing (..)


type AppError
    = UnspecifiedError
    | AccountCreationGitHubPermissionsRejected
    | AccountCreationHandleAlreadyTaken


fromString : String -> AppError
fromString s =
    case s of
        "AccountCreationGitHubPermissionsRejected" ->
            AccountCreationGitHubPermissionsRejected

        "AccountCreationHandleAlreadyTaken" ->
            AccountCreationHandleAlreadyTaken

        "UnspecifiedError" ->
            UnspecifiedError

        _ ->
            UnspecifiedError


toString : AppError -> String
toString e =
    case e of
        AccountCreationGitHubPermissionsRejected ->
            "AccountCreationGitHubPermissionsRejected"

        AccountCreationHandleAlreadyTaken ->
            "AccountCreationHandleAlreadyTaken"

        UnspecifiedError ->
            "UnspecifiedError"
