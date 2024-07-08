module UnisonShare.DefinitionDiff exposing (..)

import Code.Syntax.Linked as Linked
import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Util exposing (decodeNonEmptyList)
import List.Nonempty as NEL


type DefinitionType
    = Term
    | Type


type alias DiffSyntaxSegments =
    NEL.Nonempty SyntaxSegment


type DiffSegment
    = Old DiffSyntaxSegments
    | New DiffSyntaxSegments
    | Both DiffSyntaxSegments
    | AnnotationChange { from : SyntaxSegment, to : SyntaxSegment }
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


definitionTypeToString : DefinitionType -> String
definitionTypeToString type_ =
    case type_ of
        Term ->
            "Term"

        Type ->
            "Type"



-- VIEW


viewDiffSegment : DiffSegment -> Html msg
viewDiffSegment segment =
    let
        viewSegments : DiffSyntaxSegments -> List (Html msg)
        viewSegments segments =
            segments
                |> NEL.map (SyntaxSegment.view Linked.NotLinked)
                |> NEL.toList
    in
    case segment of
        Old segments ->
            span [ class "old" ] (viewSegments segments)

        New segments ->
            span [ class "new" ] (viewSegments segments)

        Both segments ->
            span [] (viewSegments segments)

        AnnotationChange _ ->
            span [] []

        SegmentChange _ ->
            span [] []


viewDiff : NEL.Nonempty DiffSegment -> Html msg
viewDiff segments =
    let
        segments_ =
            segments
                |> NEL.map viewDiffSegment
                |> NEL.toList
    in
    div [] segments_


view : DefinitionDiff -> Html msg
view defDiff =
    case defDiff of
        Diff _ diff ->
            div [] [ viewDiff diff ]

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

        mkAnnotationChange from to =
            AnnotationChange { from = from, to = to }

        decodeAnnotationChange =
            Decode.map2
                mkAnnotationChange
                (SyntaxSegment.decode_ { segmentField = "segment", annotationField = "fromAnnotation" })
                (SyntaxSegment.decode_ { segmentField = "segment", annotationField = "toAnnotation" })

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
