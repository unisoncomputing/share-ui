module UnisonShare.DefinitionDiff exposing (..)

import Array
import Code.Hash as Hash exposing (Hash)
import Code.Syntax.SyntaxSegment as SyntaxSegment exposing (SyntaxSegment)
import Json.Decode as Decode
import Json.Decode.Extra exposing (when)
import Json.Decode.Pipeline exposing (required, requiredAt)
import Lib.Decode.Helpers exposing (nonEmptyList, whenKindIs)
import List.Extra as ListE
import List.Nonempty as NEL


type alias DiffSyntaxSegments =
    NEL.Nonempty SyntaxSegment


type DiffSegment
    = Both DiffSyntaxSegments
    | OneSided DiffSyntaxSegments
    | AnnotationChange { segment : SyntaxSegment, fromHash : Hash, toHash : Hash }
    | SegmentChange { from : SyntaxSegment, to : SyntaxSegment }


type DiffLine
    = ChangedLine { lineNum : Int, segments : List DiffSegment }
    | UnchangedLine { lineNum : Int, segments : List DiffSegment }
      -- Spacer includes a number of lines it spans over such that we can avoid
      -- a jagged background pattern when it spans over multiple lines by
      -- making it 1 tall DOM element instead of a list of small 1 line height
      -- elements
    | Spacer { numLines : Int }


type Collapsed
    = Collapsed (List DiffLine)
    | NotCollapsed (List DiffLine)


type alias DiffDetails =
    { type_ : DefinitionType
    , left : List Collapsed
    , right : List Collapsed
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


isCollapsable : DiffLine -> Bool
isCollapsable l =
    case l of
        ChangedLine _ ->
            False

        UnchangedLine _ ->
            True

        Spacer _ ->
            False


supportsLineNum : DiffLine -> Bool
supportsLineNum l =
    case l of
        ChangedLine _ ->
            True

        UnchangedLine _ ->
            True

        Spacer _ ->
            False


toCollapsedWithLineNums : List DiffLine -> List Collapsed
toCollapsedWithLineNums lines =
    let
        arrLines =
            Array.fromList lines

        maxLineNum =
            (lines |> List.filter supportsLineNum |> List.length) + 1

        isCollapsable_ i =
            arrLines
                |> Array.get i
                |> Maybe.map isCollapsable

        prevNeighborsCollapsable i =
            case ( isCollapsable_ (i - 3), isCollapsable_ (i - 2), isCollapsable_ (i - 1) ) of
                ( Just True, Just True, Just True ) ->
                    True

                ( Nothing, Just True, Just True ) ->
                    True

                ( Nothing, Nothing, Just True ) ->
                    True

                _ ->
                    False

        nextNeighborsCollapsable i =
            case ( isCollapsable_ (i + 1), isCollapsable_ (i + 2), isCollapsable_ (i + 3) ) of
                ( Just True, Just True, Just True ) ->
                    True

                ( Just True, Just True, Nothing ) ->
                    True

                ( Just True, Nothing, Nothing ) ->
                    True

                _ ->
                    False

        withLineNum ln line =
            case line of
                UnchangedLine unchanged ->
                    -- Subtracking the line num because we are looping in reverse
                    ( ln - 1, UnchangedLine { unchanged | lineNum = ln - 1 } )

                ChangedLine changed ->
                    ( ln - 1, ChangedLine { changed | lineNum = ln - 1 } )

                Spacer spacer ->
                    ( ln, Spacer spacer )

        go i line ( ln, lines_ ) =
            let
                ( nextLn, l ) =
                    withLineNum ln line

                mergeIntoPreviousCollapsed =
                    case lines_ of
                        (Collapsed ls) :: rest ->
                            ( nextLn, Collapsed (l :: ls) :: rest )

                        _ ->
                            ( nextLn, Collapsed [ l ] :: lines_ )

                mergeIntoPreviousNotCollapsed =
                    case lines_ of
                        (NotCollapsed ls) :: rest ->
                            ( nextLn, NotCollapsed (l :: ls) :: rest )

                        _ ->
                            ( nextLn, NotCollapsed [ l ] :: lines_ )
            in
            if isCollapsable l then
                let
                    prevCollapsable =
                        prevNeighborsCollapsable i

                    nextCollapsable =
                        nextNeighborsCollapsable i

                    isFirst =
                        i == 0

                    isLast =
                        i == (Array.length arrLines - 1)
                in
                if isFirst && nextCollapsable then
                    mergeIntoPreviousCollapsed

                else if isLast && prevCollapsable then
                    mergeIntoPreviousCollapsed

                else if prevCollapsable && nextCollapsable then
                    mergeIntoPreviousCollapsed

                else
                    mergeIntoPreviousNotCollapsed

            else
                mergeIntoPreviousNotCollapsed
    in
    lines
        |> ListE.indexedFoldr go ( maxLineNum, [] )
        |> Tuple.second



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


{-| note how `lineNum` is 0. It's fixed in a second pass via a fold
because it can't be indexed based due to Spacer not having line numbers
-}
decodeDiffLine : Decode.Decoder DiffLine
decodeDiffLine =
    let
        mkChangedLine segments =
            ChangedLine { lineNum = 0, segments = segments }

        mkUnchangedLine segments =
            UnchangedLine { lineNum = 0, segments = segments }
    in
    Decode.oneOf
        [ whenKindIs "changed"
            (Decode.map mkChangedLine
                (Decode.field "value" (Decode.list decodeSegment))
            )
        , whenKindIs "unchanged"
            (Decode.map mkUnchangedLine
                (Decode.field "value" (Decode.list decodeSegment))
            )

        -- The spacer numLines will be flattened later on
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

        mergeSpacers lines =
            let
                go diffLine acc =
                    case diffLine of
                        Spacer spacer ->
                            case ListE.unconsLast acc of
                                Just ( Spacer prevSpacer, lines_ ) ->
                                    lines_ ++ [ Spacer { numLines = prevSpacer.numLines + spacer.numLines } ]

                                _ ->
                                    acc ++ [ diffLine ]

                        _ ->
                            acc ++ [ diffLine ]
            in
            List.foldl go [] lines
    in
    Decode.succeed mkDiff
        |> requiredAt [ "diff", "diff", "contents", "left" ]
            (Decode.map (mergeSpacers >> toCollapsedWithLineNums) (Decode.list decodeDiffLine))
        |> requiredAt [ "diff", "diff", "contents", "right" ]
            (Decode.map (mergeSpacers >> toCollapsedWithLineNums) (Decode.list decodeDiffLine))


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
