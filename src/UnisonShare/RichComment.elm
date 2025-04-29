module UnisonShare.RichComment exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import RemoteData exposing (RemoteData(..), WebData)
import UnisonShare.AppContext exposing (AppContext)
import UnisonShare.BranchDiff.ChangeLine exposing (ChangeLine)
import UnisonShare.BranchDiff.ChangeLineId exposing (ChangeLineId)
import UnisonShare.Contribution.ContributionRef exposing (ContributionRef)
import UnisonShare.Markdown as Markdown
import UnisonShare.Project.ProjectRef exposing (ProjectRef)
import UnisonShare.Route as Route
import Url exposing (Url)



-- MODEL


type CommentPart
    = Text String
    | EmbedContributionChangeLine
        { url : Url
        , projectRef : ProjectRef
        , contribRef : ContributionRef
        , changeLineId : ChangeLineId
        , changeLine : WebData ChangeLine
        }


type alias RichComment =
    List CommentPart


type alias Model =
    RichComment


lines : String -> List String
lines =
    String.split "\n"


words : String -> List String
words =
    String.split " "


init : AppContext -> String -> ( Model, Cmd Msg )
init appContext comment =
    let
        toCommentPart word =
            case word |> Url.fromString of
                Just url ->
                    case Route.fromUrl appContext.basePath url of
                        Route.Project projectRef (Route.ProjectContribution contribRef (Route.ProjectContributionChanges (Just changeLineId))) ->
                            EmbedContributionChangeLine
                                { url = url
                                , projectRef = projectRef
                                , contribRef = contribRef
                                , changeLineId = changeLineId
                                , changeLine = Loading
                                }

                        _ ->
                            Text word

                _ ->
                    Text word

        mergeLines line ( comment_, cmds_ ) =
            let
                embedChangeLineUrl w =
                    case w of
                        EmbedContributionChangeLine { projectRef, contribRef, changeLineId } ->
                            Just (fetchChangeLine projectRef contribRef changeLineId)

                        _ ->
                            Nothing

                cmds__ =
                    line
                        |> List.filterMap embedChangeLineUrl
            in
            if List.isEmpty comment_ then
                ( line, cmds_ ++ cmds__ )

            else
                ( comment_ ++ (Text "\n" :: line), cmds_ ++ cmds__ )

        ( model, cmds ) =
            comment
                |> lines
                |> List.map words
                |> List.map (List.map toCommentPart)
                |> List.foldl mergeLines ( [], [] )
    in
    ( model, Cmd.batch cmds )



-- UPDATE


type Msg
    = FetchChangeLineFinished


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- EFFECTS


fetchChangeLine : ProjectRef -> ContributionRef -> ChangeLineId -> Cmd Msg
fetchChangeLine _ _ _ =
    -- Fetch BranchDiff, then fetch ChangeLine
    Cmd.none



-- VIEW


viewCommentPart : CommentPart -> Html Msg
viewCommentPart cp =
    case cp of
        Text t ->
            Markdown.view t

        EmbedContributionChangeLine { url, changeLine } ->
            case changeLine of
                Success _ ->
                    div [] [ text "Some changeline snippet" ]

                _ ->
                    Markdown.view (Url.toString url)


view : Model -> Html Msg
view model =
    div [ class "rich-comment" ] (List.map viewCommentPart model)
