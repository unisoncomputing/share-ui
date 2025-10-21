module UnisonShare.DefinitionDiffCard exposing (..)

import Code.Hash as Hash
import Code.Syntax.SyntaxConfig exposing (SyntaxConfig)
import Code.Syntax.SyntaxSegment as SyntaxSegment
import Html exposing (Html, code, div, header, pre, span, text)
import Html.Attributes exposing (class, style)
import List.Nonempty as NEL
import String.Extra exposing (pluralize)
import UI.Tooltip as Tooltip
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (DefinitionDiff(..), DiffDetails, DiffLine(..), DiffSegment(..))



-- VIEW


viewSegments : SyntaxConfig msg -> String -> NEL.Nonempty SyntaxSegment.SyntaxSegment -> List (Html msg)
viewSegments syntaxConfig className segments =
    segments
        |> NEL.map (SyntaxSegment.view syntaxConfig)
        |> NEL.map (\seg -> span [ class className ] [ seg ])
        |> NEL.toList


viewTooltip : Html msg -> Tooltip.Tooltip msg
viewTooltip content =
    Tooltip.rich content
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start


viewDiffSegment : SyntaxConfig msg -> DiffSegment -> List (Html msg)
viewDiffSegment syntaxConfig segment =
    let
        viewSegment =
            SyntaxSegment.view syntaxConfig

        viewSegments_ className =
            viewSegments syntaxConfig className
    in
    case segment of
        Both segments ->
            viewSegments_ "diff-segment both" segments

        OneSided segments ->
            viewSegments_ "diff-segment one-sided" segments

        AnnotationChange change ->
            [ viewTooltip
                (div [ class "tooltip-changes-summary" ]
                    [ div [ class "hash-changed" ]
                        [ text "The hash changed"
                        , text " from "
                        , Hash.view change.fromHash
                        , text " to "
                        , Hash.view change.toHash
                        ]
                    ]
                )
                |> Tooltip.view
                    (span [ class "diff-segment annotation-change" ] [ viewSegment change.segment ])
            ]

        SegmentChange { from, to } ->
            [ viewTooltip
                (div [ class "tooltip-changes-summary" ]
                    [ text "Changed from"
                    , code [] [ viewSegment from ]
                    ]
                )
                |> Tooltip.view
                    (span [ class "diff-segment segment-change" ] [ viewSegment to ])
            ]


viewDiffLine : (DiffSegment -> List (Html msg)) -> String -> Int -> DiffLine -> Html msg
viewDiffLine viewSeg changeIndicator gutterWidth line =
    let
        gutter ln indicator =
            span [ class "gutter" ]
                [ span [ class "line-number" ]
                    [ text
                        (String.padLeft
                            gutterWidth
                            ' '
                            (String.fromInt ln)
                        )
                    ]
                , text " "
                , span [ class "change-indicator" ] [ text indicator ]
                , text " "
                ]
    in
    case line of
        ChangedLine { lineNum, segments } ->
            div [ class "diff-line changed-line" ]
                [ gutter lineNum changeIndicator
                , span [ class "diff-line_syntax" ] (List.concatMap viewSeg segments)
                ]

        UnchangedLine { lineNum, segments } ->
            div [ class "diff-line unchanged-line" ]
                [ gutter lineNum " "
                , span [ class "diff-line_syntax" ] (List.concatMap viewSeg segments)
                ]

        Spacer { numLines } ->
            div
                [ class "diff-line spacer-line"
                , style "height" ("calc(var(--diff-line-height) * " ++ String.fromInt numLines ++ ")")
                ]
                []


viewCollapsed : (DiffLine -> Html msg) -> DefinitionDiff.Collapsed -> Html msg
viewCollapsed viewLine collapsed =
    case collapsed of
        DefinitionDiff.Collapsed lines ->
            let
                numCollapsedLines =
                    List.length lines
            in
            div [ class "collapsed-section" ]
                [ text
                    (String.fromInt numCollapsedLines
                        ++ pluralize " line " " lines " numCollapsedLines
                        ++ "hidden..."
                    )
                ]

        DefinitionDiff.NotCollapsed lines ->
            div [] (List.map viewLine lines)


diffLength : List DefinitionDiff.Collapsed -> Int
diffLength collapsedLines =
    let
        toLengths l =
            case l of
                DefinitionDiff.Collapsed lines ->
                    List.length lines

                DefinitionDiff.NotCollapsed lines ->
                    List.length lines
    in
    collapsedLines
        |> List.map toLengths
        |> List.sum


viewDiff : (Bool -> SyntaxConfig msg) -> DiffDetails -> Html msg
viewDiff toSyntaxConfig { left, right } =
    let
        toGutterWidth len =
            String.length (String.fromInt len)

        toViewDiffSegment isNew =
            viewDiffSegment (toSyntaxConfig isNew)

        viewLeftDiffLine =
            viewDiffLine (toViewDiffSegment False)
                "-"
                (toGutterWidth (diffLength left))

        viewRightDiffLine =
            viewDiffLine (toViewDiffSegment True)
                "+"
                (toGutterWidth (diffLength right))

        before =
            List.map (viewCollapsed viewLeftDiffLine) left

        after =
            List.map (viewCollapsed viewRightDiffLine) right
    in
    div [ class "diff-side-by-side" ]
        [ pre [ class "monochrome diff-side left" ]
            [ header [ class "diff-left-header" ] [ text "Before" ]
            , code [] before
            ]
        , pre [ class "monochrome diff-side right" ]
            [ header [ class "diff-right-header" ] [ text "After" ]
            , code [] after
            ]
        ]


view : (Bool -> SyntaxConfig msg) -> DefinitionDiff -> Html msg
view toSyntaxConfig defDiff =
    case defDiff of
        Diff details ->
            div [] [ viewDiff toSyntaxConfig details ]

        Mismatched { left, right } ->
            div [ class "diff-side-by-side" ]
                [ pre [ class "monochrome diff-side" ] [ code [] (viewSegments (toSyntaxConfig False) "mismatched old" left) ]
                , pre [ class "monochrome diff-side" ] [ code [] (viewSegments (toSyntaxConfig True) "mismatched new" right) ]
                ]
