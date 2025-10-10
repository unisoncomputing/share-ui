module UnisonShare.DefinitionDiff exposing (..)

import Code.Hash as Hash exposing (Hash)
import Code.Syntax.SyntaxConfig exposing (SyntaxConfig)
import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
import Html exposing (Html, code, div, header, pre, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (nonEmptyList, whenKindIs)
import List.Nonempty as NEL
import UI.Tooltip as Tooltip


type alias DiffSyntaxSegments =
    NEL.Nonempty SyntaxSegment


type DiffSegment
    = Old DiffSyntaxSegments
    | New DiffSyntaxSegments
    | Both DiffSyntaxSegments
    | AnnotationChange { segment : SyntaxSegment, fromHash : Hash, toHash : Hash }
    | SegmentChange { from : SyntaxSegment, to : SyntaxSegment }


type DiffLine
    = ChangedLine (List DiffSegment)
    | UnchangedLine (List DiffSegment)
    | SpacerLine


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


{-| View diff segments from the perspective of viewing the old definition
-}
viewOldDiffSegment : SyntaxConfig msg -> DiffSegment -> List (Html msg)
viewOldDiffSegment syntaxConfig segment =
    let
        viewSegment =
            SyntaxSegment.view syntaxConfig

        viewSegments_ className =
            viewSegments syntaxConfig className
    in
    case segment of
        Old segments ->
            viewSegments_ "diff-segment old" segments

        Both segments ->
            viewSegments_ "diff-segment both" segments

        New _ ->
            []

        AnnotationChange change ->
            [ viewSegment change.segment ]

        SegmentChange { from } ->
            [ viewSegment from ]


{-| View diff segments from the perspective of viewing the new definition
-}
viewNewDiffSegment : SyntaxConfig msg -> DiffSegment -> List (Html msg)
viewNewDiffSegment syntaxConfig segment =
    let
        viewSegment =
            SyntaxSegment.view syntaxConfig

        viewSegments_ className =
            viewSegments syntaxConfig className
    in
    case segment of
        Old _ ->
            []

        New segments ->
            viewSegments_ "diff-segment new" segments

        Both segments ->
            viewSegments_ "diff-segment both" segments

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


viewDiffLine : (DiffSegment -> List (Html msg)) -> DiffLine -> Html msg
viewDiffLine viewSeg line =
    case line of
        ChangedLine segments ->
            div [ class "diff-line changed-line" ] (List.concatMap viewSeg segments)

        UnchangedLine segments ->
            div [ class "diff-line unchanged-line" ] (List.concatMap viewSeg segments)

        SpacerLine ->
            div [ class "diff-line spacer-line" ] []


viewDiff : (Bool -> SyntaxConfig msg) -> DiffDetails -> Html msg
viewDiff toSyntaxConfig { left, right } =
    let
        before =
            left
                |> List.map (viewDiffLine (viewOldDiffSegment (toSyntaxConfig False)))

        after =
            right
                |> List.map (viewDiffLine (viewNewDiffSegment (toSyntaxConfig True)))
    in
    div [ class "diff-side-by-side" ]
        [ pre [ class "monochrome diff-side old" ]
            [ header [ class "diff-old-header" ] [ text "Before" ]
            , code [] before
            ]
        , pre [ class "monochrome diff-side new" ]
            [ header [ class "diff-new-header" ] [ text "After" ]
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


decodeSegment : Decode.Decoder DiffSegment
decodeSegment =
    let
        decodeDiffTag =
            Decode.field "diffTag" Decode.string

        decodeOld =
            Decode.succeed Old
                |> required "elements" decodeDiffSyntaxSegments

        decodeNew =
            Decode.succeed New
                |> required "elements" decodeDiffSyntaxSegments

        decodeBoth =
            Decode.succeed Both
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
        [ when decodeDiffTag ((==) "old") decodeOld
        , when decodeDiffTag ((==) "new") decodeNew
        , when decodeDiffTag ((==) "both") decodeBoth
        , when decodeDiffTag ((==) "annotationChange") decodeAnnotationChange
        , when decodeDiffTag ((==) "segmentChange") decodeSegmentChange
        ]


decodeDiffLine : Decode.Decoder DiffLine
decodeDiffLine =
    Decode.oneOf
        [ whenKindIs "changed" (Decode.map ChangedLine (Decode.field "value" (Decode.list decodeSegment)))
        , whenKindIs "unchanged" (Decode.map UnchangedLine (Decode.field "value" (Decode.list decodeSegment)))
        , whenKindIs "spacer" (Decode.succeed SpacerLine)
        ]


decodeDiff : DefinitionType -> Decode.Decoder DefinitionDiff
decodeDiff definitionType =
    let
        definitionKey =
            case definitionType of
                Term ->
                    "termDefinition"

                Type ->
                    "typeDefinition"

        mkDiff left right =
            Diff
                { type_ = definitionType
                , left = left
                , right = right
                }
    in
    Decode.succeed mkDiff
        |> requiredAt [ "left", definitionKey, "contents" ] (Decode.list decodeDiffLine)
        |> requiredAt [ "right", definitionKey, "contents" ] (Decode.list decodeDiffLine)


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
