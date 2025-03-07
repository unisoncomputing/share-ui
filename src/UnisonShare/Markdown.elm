module UnisonShare.Markdown exposing (view, view_)

import Html exposing (Attribute, Html, code, div, pre, span, text)
import Html.Attributes exposing (class)
import Markdown.Parser as MdParser
import Markdown.Renderer exposing (defaultHtmlRenderer)
import UnisonShare.Link as Link


viewCodeBlock : { body : String, language : Maybe String } -> Html msg
viewCodeBlock { body, language } =
    let
        langAttr =
            language
                |> Maybe.map (\l -> [ class ("language-" ++ l) ])
                |> Maybe.withDefault []
    in
    pre (class "source code" :: langAttr) [ code [] [ text body ] ]


{-| TODO: UnisonShare.Link (UI.Click really) should support "title"
-}
viewLink : { title : Maybe String, destination : String } -> List (Html msg) -> Html msg
viewLink { destination } content =
    Link.link destination
        |> Link.view_ (span [] content)


view_ : List (Attribute msg) -> String -> Html msg
view_ attrs rawMarkdown =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map MdParser.deadEndToString
                |> String.join "\n"

        renderer =
            { defaultHtmlRenderer | link = viewLink, codeBlock = viewCodeBlock }

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
