module UnisonShare.DefinitionDiffCard exposing (..)

import Code.Hash as Hash
import Code.Syntax.SyntaxConfig exposing (SyntaxConfig)
import Code.Syntax.SyntaxSegment as SyntaxSegment
import Html exposing (Html, code, div, header, pre, span, text)
import Html.Attributes exposing (class, style)
import List.Nonempty as NEL
import String.Extra exposing (pluralize)
import UI.Click as Click
import UI.Icon as Icon
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


viewGutter : Int -> Int -> String -> Html msg
viewGutter gutterWidth ln indicator =
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


viewDiffLine : (DiffSegment -> List (Html msg)) -> String -> Int -> DiffLine -> Html msg
viewDiffLine viewSeg changeIndicator gutterWidth line =
    let
        gutter ln indicator =
            viewGutter gutterWidth ln indicator
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


viewCollapsed : ViewConfig msg -> (DiffLine -> Html msg) -> Int -> DefinitionDiff.Collapsed -> Html msg
viewCollapsed cfg viewLine index collapsed =
    case collapsed of
        DefinitionDiff.Collapsed lines ->
            let
                numCollapsedLines =
                    List.length lines
            in
            Click.onClick (cfg.toExpandCollapsedDiffSectionMsg { index = index })
                |> Click.view
                    [ class "collapsed-section" ]
                    [ Icon.view Icon.dots
                    , text
                        (pluralize "line " "lines " numCollapsedLines
                            ++ "hidden"
                        )
                    , Icon.view Icon.dots
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


viewDiff : ViewConfig msg -> DiffDetails -> Html msg
viewDiff cfg { left, right } =
    let
        toGutterWidth len =
            String.length (String.fromInt len)

        toViewDiffSegment isNew =
            viewDiffSegment (cfg.toSyntaxConfig isNew)

        viewLeftDiffLine =
            viewDiffLine (toViewDiffSegment False)
                "-"
                (toGutterWidth (diffLength left))

        viewRightDiffLine =
            viewDiffLine (toViewDiffSegment True)
                "+"
                (toGutterWidth (diffLength right))

        before =
            List.indexedMap (viewCollapsed cfg viewLeftDiffLine) left

        after =
            List.indexedMap (viewCollapsed cfg viewRightDiffLine) right
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


type alias ViewConfig msg =
    { toSyntaxConfig : Bool -> SyntaxConfig msg
    , toExpandCollapsedDiffSectionMsg : { index : Int } -> msg
    }


view : ViewConfig msg -> DefinitionDiff -> Html msg
view cfg defDiff =
    case defDiff of
        Diff details ->
            div [] [ viewDiff cfg details ]

        Mismatched { left, right } ->
            div [ class "diff-side-by-side" ]
                [ pre [ class "monochrome diff-side" ] [ code [] (viewSegments (cfg.toSyntaxConfig False) "mismatched old" left) ]
                , pre [ class "monochrome diff-side" ] [ code [] (viewSegments (cfg.toSyntaxConfig True) "mismatched new" right) ]
                ]
