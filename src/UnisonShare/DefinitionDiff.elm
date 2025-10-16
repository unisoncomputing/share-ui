module UnisonShare.DefinitionDiff exposing (..)

import Code.Hash as Hash exposing (Hash)
import Code.Syntax.SyntaxConfig exposing (SyntaxConfig)
import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
import Html exposing (Html, code, div, header, pre, span, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (nonEmptyList, whenKindIs)
import List.Extra as ListE
import List.Nonempty as NEL
import UI.Tooltip as Tooltip


type alias DiffSyntaxSegments =
    NEL.Nonempty SyntaxSegment


type DiffSegment
    = Both DiffSyntaxSegments
    | OneSided DiffSyntaxSegments
    | AnnotationChange { segment : SyntaxSegment, fromHash : Hash, toHash : Hash }
    | SegmentChange { from : SyntaxSegment, to : SyntaxSegment }


type DiffLine
    = ChangedLine (List DiffSegment)
    | UnchangedLine (List DiffSegment)
      -- Spacer includes numLines such that we can avoid a jagged background
      -- pattern when it spans over multiple lines by making it 1 tall DOM
      -- element instead of small 1 line height elements
    | Spacer { numLines : Int }


type alias DiffDetails =
    { type_ : DefinitionType
    , left : List DiffLine
    , right : List DiffLine
    }


type alias MismatchedDetails =
    { type_ : DefinitionType
    , left : DiffSyntaxSegments
    , right : DiffSyntaxSegments
    }


type DefinitionDiff
    = Diff DiffDetails
    | Mismatched MismatchedDetails



-- Definition Type


type DefinitionType
    = Term
    | Type


definitionTypeToString : DefinitionType -> String
definitionTypeToString type_ =
    case type_ of
        Term ->
            "Term"

        Type ->
            "Type"



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


viewDiffLine : (DiffSegment -> List (Html msg)) -> String -> Int -> ( Maybe Int, DiffLine ) -> Html msg
viewDiffLine viewSeg changeIndicator gutterWidth ( ln, line ) =
    let
        gutter indicator =
            span [ class "gutter" ]
                [ span [ class "line-number" ]
                    [ text
                        (String.padLeft
                            gutterWidth
                            ' '
                            (ln |> Maybe.map String.fromInt |> Maybe.withDefault "")
                        )
                    ]
                , text " "
                , span [ class "change-indicator" ] [ text indicator ]
                , text " "
                ]
    in
    case line of
        ChangedLine segments ->
            div [ class "diff-line changed-line" ]
                [ gutter changeIndicator
                , span [] (List.concatMap viewSeg segments)
                ]

        UnchangedLine segments ->
            div [ class "diff-line unchanged-line" ]
                [ gutter " "
                , span [] (List.concatMap viewSeg segments)
                ]

        Spacer { numLines } ->
            div
                [ class "diff-line spacer-line"
                , style "height" ("calc(var(--diff-line-height) * " ++ String.fromInt numLines ++ ")")
                ]
                []


viewDiff : (Bool -> SyntaxConfig msg) -> DiffDetails -> Html msg
viewDiff toSyntaxConfig { left, right } =
    let
        toGutterWidth len =
            String.length (String.fromInt len)

        toViewDiffSegment isNew =
            viewDiffSegment (toSyntaxConfig isNew)

        withLineNumbers diffLine ( i, lines ) =
            case diffLine of
                ChangedLine _ ->
                    ( i + 1, lines ++ [ ( Just (i + 1), diffLine ) ] )

                UnchangedLine _ ->
                    ( i + 1, lines ++ [ ( Just (i + 1), diffLine ) ] )

                Spacer _ ->
                    case ListE.unconsLast lines of
                        Just ( ( _, Spacer { numLines } ), lines_ ) ->
                            ( i, lines_ ++ [ ( Nothing, Spacer { numLines = numLines + 1 } ) ] )

                        _ ->
                            ( i, lines ++ [ ( Nothing, diffLine ) ] )

        viewLeftDiffLine =
            viewDiffLine (toViewDiffSegment False) "-" (toGutterWidth (List.length left))

        viewRightDiffLine =
            viewDiffLine (toViewDiffSegment False) "+" (toGutterWidth (List.length right))

        before =
            left
                |> List.foldl withLineNumbers ( 0, [] )
                |> Tuple.second
                |> List.map viewLeftDiffLine

        after =
            right
                |> List.foldl withLineNumbers ( 0, [] )
                |> Tuple.second
                |> List.map viewRightDiffLine
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



-- DECODE


decodeDiffSyntaxSegments : Decode.Decoder DiffSyntaxSegments
decodeDiffSyntaxSegments =
    nonEmptyList SyntaxSegment.decode


decodeSingleDiffSyntaxSegment : Decode.Decoder DiffSyntaxSegments
decodeSingleDiffSyntaxSegment =
    Decode.map NEL.fromElement SyntaxSegment.decode


decodeSegment : Decode.Decoder DiffSegment
decodeSegment =
    let
        decodeDiffTag =
            Decode.field "diffTag" Decode.string

        decodeBoth =
            Decode.succeed Both
                |> required "elements" decodeDiffSyntaxSegments

        decodeOneSided =
            Decode.succeed OneSided
                |> required "elements" decodeDiffSyntaxSegments

        mkAnnotationChange segment fromHash toHash =
            AnnotationChange
                { segment = segment
                , fromHash = fromHash
                , toHash = toHash
                }

        decodeAnnotationChange =
            Decode.map3
                mkAnnotationChange
                (SyntaxSegment.decode_ { segmentField = "segment", annotationField = "fromAnnotation" })
                (Decode.at [ "fromAnnotation", "contents" ] Hash.decode)
                (Decode.at [ "toAnnotation", "contents" ] Hash.decode)

        mkSegmentChange from to =
            SegmentChange { from = from, to = to }

        decodeSegmentChange =
            Decode.map2
                mkSegmentChange
                (SyntaxSegment.decode_ { segmentField = "fromSegment", annotationField = "annotation" })
                (SyntaxSegment.decode_ { segmentField = "toSegment", annotationField = "annotation" })
    in
    Decode.oneOf
        [ when decodeDiffTag ((==) "both") decodeBoth
        , when decodeDiffTag ((==) "oneSided") decodeOneSided
        , when decodeDiffTag ((==) "annotationChange") decodeAnnotationChange
        , when decodeDiffTag ((==) "segmentChange") decodeSegmentChange
        ]


decodeDiffLine : Decode.Decoder DiffLine
decodeDiffLine =
    Decode.oneOf
        [ whenKindIs "changed" (Decode.map ChangedLine (Decode.field "value" (Decode.list decodeSegment)))
        , whenKindIs "unchanged" (Decode.map UnchangedLine (Decode.field "value" (Decode.list decodeSegment)))

        -- The spacer numLines will be flatten later on
        -- TODO: we should probably do the flattening and add line numbers during parsing...
        , whenKindIs "spacer" (Decode.succeed (Spacer { numLines = 1 }))
        ]


decodeDiff : DefinitionType -> Decode.Decoder DefinitionDiff
decodeDiff definitionType =
    let
        mkDiff left right =
            Diff
                { type_ = definitionType
                , left = left
                , right = right
                }
    in
    Decode.succeed mkDiff
        |> requiredAt [ "diff", "diff", "contents", "left" ] (Decode.list decodeDiffLine)
        |> requiredAt [ "diff", "diff", "contents", "right" ] (Decode.list decodeDiffLine)


decodeMismatched : DefinitionType -> Decode.Decoder DefinitionDiff
decodeMismatched definitionType =
    let
        definitionKey =
            case definitionType of
                Term ->
                    "termDefinition"

                Type ->
                    "typeDefinition"

        mkMismatched left right =
            Mismatched
                { type_ = definitionType
                , left = left
                , right = right
                }
    in
    Decode.succeed mkMismatched
        -- TODO: what about builtins?
        |> requiredAt [ "left", definitionKey, "contents" ] decodeDiffSyntaxSegments
        |> requiredAt [ "right", definitionKey, "contents" ] decodeDiffSyntaxSegments


decode : DefinitionType -> Decode.Decoder DefinitionDiff
decode definitionType =
    let
        decodeKind =
            Decode.at [ "diff", "diffKind" ] Decode.string
    in
    Decode.oneOf
        [ when decodeKind ((==) "diff") (decodeDiff definitionType)
        , when decodeKind ((==) "mismatched") (decodeMismatched definitionType)
        ]
