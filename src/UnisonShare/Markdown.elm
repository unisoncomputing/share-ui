module UnisonShare.Markdown exposing (view, view_)

import Html exposing (Attribute, Html, code, div, pre, text)
import Html.Attributes exposing (class)
import Markdown.Parser as MdParser
import Markdown.Renderer exposing (defaultHtmlRenderer)


viewCodeBlock : { body : String, language : Maybe String } -> Html msg
viewCodeBlock { body, language } =
    let
        langAttr =
            language
                |> Maybe.map (\l -> [ class ("language-" ++ l) ])
                |> Maybe.withDefault []
    in
    pre (class "source code" :: langAttr) [ code [] [ text body ] ]


view_ : List (Attribute msg) -> String -> Html msg
view_ attrs rawMarkdown =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map MdParser.deadEndToString
                |> String.join "\n"

        renderer =
            { defaultHtmlRenderer | codeBlock = viewCodeBlock }

        content =
            case
                rawMarkdown
                    |> MdParser.parse
                    |> Result.mapError deadEndsToString
                    |> Result.andThen (Markdown.Renderer.render renderer)
            of
                Ok md ->
                    md

                Err _ ->
                    [ text rawMarkdown ]
    in
    div attrs content


view : String -> Html msg
view rawMarkdown =
    view_ [ class "definition-doc" ] rawMarkdown
