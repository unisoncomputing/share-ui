module UnisonShare.DefinitionDiff exposing (..)

import Code.Hash as Hash exposing (Hash)
import Code.Syntax.Linked as Linked
import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
import Html exposing (Html, code, div, pre, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Util exposing (decodeNonEmptyList)
import List.Nonempty as NEL
import UI
import UI.Tooltip as Tooltip


type alias DiffSyntaxSegments =
    NEL.Nonempty SyntaxSegment


type DiffSegment
    = Old DiffSyntaxSegments
    | New DiffSyntaxSegments
    | Both DiffSyntaxSegments
    | AnnotationChange { segment : SyntaxSegment, fromHash : Hash, toHash : Hash }
    | SegmentChange { from : SyntaxSegment, to : SyntaxSegment }


type alias DiffDetails =
    { type_ : DefinitionType
    , newDef : DiffSyntaxSegments
    , oldDef : DiffSyntaxSegments
    }


type DefinitionDiff
    = Diff DiffDetails (NEL.Nonempty DiffSegment)
    | Mismatched DiffDetails



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


viewSegments : Linked.Linked msg -> NEL.Nonempty SyntaxSegment.SyntaxSegment -> List (Html msg)
viewSegments linked segments =
    segments
        |> NEL.map (SyntaxSegment.view linked)
        |> NEL.toList


viewTooltip : Html msg -> Tooltip.Tooltip msg
viewTooltip content =
    Tooltip.rich content
        |> Tooltip.tooltip
        |> Tooltip.withArrow Tooltip.Start


{-| View diff segments from the perspective of viewing the old definition
-}
viewOldDiffSegment : Linked.Linked msg -> DiffSegment -> Html msg
viewOldDiffSegment linked segment =
    let
        viewSegment =
            SyntaxSegment.view linked

        viewSegments_ =
            viewSegments linked
    in
    case segment of
        Old segments ->
            span [ class "diff-segment old" ] (viewSegments_ segments)

        Both segments ->
            span [] (viewSegments_ segments)

        New _ ->
            UI.nothing

        AnnotationChange change ->
            span [] [ viewSegment change.segment ]

        SegmentChange { from } ->
            span [] [ viewSegment from ]


{-| View diff segments from the perspective of viewing the new definition
-}
viewNewDiffSegment : Linked.Linked msg -> DiffSegment -> Html msg
viewNewDiffSegment linked segment =
    let
        viewSegment =
            SyntaxSegment.view linked

        viewSegments_ =
            viewSegments linked
    in
    case segment of
        Old _ ->
            UI.nothing

        New segments ->
            span [ class "diff-segment new" ] (viewSegments_ segments)

        Both segments ->
            span [] (viewSegments_ segments)

        AnnotationChange change ->
            viewTooltip
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

        SegmentChange { from, to } ->
            viewTooltip
                (div [ class "tooltip-changes-summary" ]
                    [ text "Changed from"
                    , code [] [ viewSegment from ]
                    ]
                )
                |> Tooltip.view
                    (span [ class "diff-segment segment-change" ] [ viewSegment to ])


viewDiff : Linked.Linked msg -> NEL.Nonempty DiffSegment -> Html msg
viewDiff linked segments =
    let
        old =
            segments
                |> NEL.map (viewOldDiffSegment linked)
                |> NEL.toList

        new =
            segments
                |> NEL.map (viewNewDiffSegment linked)
                |> NEL.toList
    in
    div [ class "diff-side-by-side" ]
        [ pre [ class "monochrome diff-side" ] [ code [] old ]
        , pre [ class "monochrome diff-side" ] [ code [] new ]
        ]


view : Linked.Linked msg -> DefinitionDiff -> Html msg
view linked defDiff =
    case defDiff of
        Diff _ diff ->
            div [] [ viewDiff linked diff ]

        Mismatched _ ->
            div [] [ text "TODO" ]



-- DECODE


decodeDiffSyntaxSegments : Decode.Decoder DiffSyntaxSegments
decodeDiffSyntaxSegments =
    decodeNonEmptyList SyntaxSegment.decode


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


decodeDiff : DefinitionType -> Decode.Decoder DefinitionDiff
decodeDiff definitionType =
    let
        ( oldKey, newKey, definitionKey ) =
            case definitionType of
                Term ->
                    ( "oldTerm", "newTerm", "termDefinition" )

                Type ->
                    ( "oldType", "newType", "typeDefinition" )

        mkDiff diff oldDef newDef =
            Diff
                { type_ = definitionType
                , oldDef = oldDef
                , newDef = newDef
                }
                diff
    in
    Decode.succeed mkDiff
        |> requiredAt [ "diff", "contents" ] (decodeNonEmptyList decodeSegment)
        |> requiredAt [ oldKey, definitionKey, "contents" ] decodeDiffSyntaxSegments
        |> requiredAt [ newKey, definitionKey, "contents" ] decodeDiffSyntaxSegments


decodeMismatched : DefinitionType -> Decode.Decoder DefinitionDiff
decodeMismatched definitionType =
    let
        ( oldKey, newKey, definitionKey ) =
            case definitionType of
                Term ->
                    ( "oldTerm", "newTerm", "termDefinition" )

                Type ->
                    ( "oldType", "newType", "typeDefinition" )

        mkMismatched oldDef newDef =
            Mismatched
                { type_ = definitionType
                , oldDef = oldDef
                , newDef = newDef
                }
    in
    Decode.succeed mkMismatched
        -- TODO: what about builtins?
        |> requiredAt [ oldKey, definitionKey, "contents" ] decodeDiffSyntaxSegments
        |> requiredAt [ newKey, definitionKey, "contents" ] decodeDiffSyntaxSegments


decode : DefinitionType -> Decode.Decoder DefinitionDiff
decode definitionType =
    let
        decodeKind =
            Decode.field "diffKind" Decode.string
    in
    Decode.oneOf
        [ when decodeKind ((==) "diff") (decodeDiff definitionType)
        , when decodeKind ((==) "mismatched") (decodeMismatched definitionType)
        ]
