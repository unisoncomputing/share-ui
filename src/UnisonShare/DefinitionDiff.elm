module UnisonShare.DefinitionDiff exposing (..)

import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
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


type DefinitionDiff
    = Diff
        { type_ : DefinitionType
        , diff : NEL.Nonempty DiffSegment
        , new : DiffSyntaxSegments
        , old : DiffSyntaxSegments
        }
    | Mismatched
        { type_ : DefinitionType
        , new : DiffSyntaxSegments
        , old : DiffSyntaxSegments
        }


definitionTypeToString : DefinitionType -> String
definitionTypeToString type_ =
    case type_ of
        Term ->
            "Term"

        Type ->
            "Type"



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

        mkDiff diff old new =
            Diff { type_ = definitionType, diff = diff, old = old, new = new }
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

        mkMismatched old new =
            Mismatched { type_ = definitionType, old = old, new = new }
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
