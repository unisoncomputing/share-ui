module UnisonShare.UcmCommand exposing (..)

import Code.BranchRef as BranchRef exposing (BranchRef)
import UnisonShare.Project.ProjectRef as ProjectRef exposing (ProjectRef)


type UcmCommand
    = Install ProjectRef (Maybe BranchRef)
    | Pull ProjectRef (Maybe BranchRef)
    | Push ProjectRef (Maybe BranchRef)


toString : UcmCommand -> String
toString command =
    case command of
        Install pr (Just br) ->
            "lib.install " ++ ProjectRef.toString pr ++ "/" ++ BranchRef.toString br

        Install pr Nothing ->
            "lib.install " ++ ProjectRef.toString pr

        Pull pr (Just br) ->
            "pull " ++ ProjectRef.toString pr ++ "/" ++ BranchRef.toString br

        Pull pr Nothing ->
            "pull " ++ ProjectRef.toString pr

        Push pr (Just br) ->
            "push " ++ ProjectRef.toString pr ++ "/" ++ BranchRef.toString br

        Push pr Nothing ->
            "push " ++ ProjectRef.toString pr
