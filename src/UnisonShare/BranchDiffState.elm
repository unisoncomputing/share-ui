module UnisonShare.BranchDiffState exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Http
import Json.Decode as Decode exposing (Decoder)
import Lib.Decode.Helpers exposing (whenFieldIs, whenPathIs)
import UnisonShare.BranchDiff as BranchDiff exposing (BranchDiff)


type DiffErrorCulprit
    = TargetBranch
    | SourceBranch


type DiffError
    = ImpossibleError
    | ConstructorAlias
        { culprit : DiffErrorCulprit
        , typeName : FQN
        , constructorName1 : FQN
        , constructorName2 : FQN
        }
    | MissingConstructorName { culprit : DiffErrorCulprit, typeName : FQN }
    | NestedDeclAlias
        { culprit : DiffErrorCulprit
        , constructorName1 : FQN
        , constructorName2 : FQN
        }
    | StrayConstructor { culprit : DiffErrorCulprit, constructorName : FQN }
    | LibFoundAtUnexpectedPath { path : FQN }
    | UnknownError


type BranchDiffState
    = Loading
    | Computing { numTries : Int }
    | Reloading { numtries : Int }
    | Computed BranchDiff
    | Uncomputable DiffError
    | Failure Http.Error



-- DECODE


whenTagIs : String -> Decode.Decoder a -> Decode.Decoder a
whenTagIs val =
    whenFieldIs "tag" val


decodeDiffError : Decoder DiffError
decodeDiffError =
    let
        makeConstructorAlias culprit typeName ctor1 ctor2 =
            ConstructorAlias
                { culprit = culprit
                , typeName = typeName
                , constructorName1 = ctor1
                , constructorName2 = ctor2
                }

        makeMissingConstructorName culprit typeName =
            MissingConstructorName
                { culprit = culprit, typeName = typeName }

        makeNestedDeclAlias culprit ctor1 ctor2 =
            NestedDeclAlias
                { culprit = culprit
                , constructorName1 = ctor1
                , constructorName2 = ctor2
                }

        makeStrayConstructor culprit ctor =
            StrayConstructor
                { culprit = culprit, constructorName = ctor }

        makeLibFoundAtUnexpectedPath path =
            LibFoundAtUnexpectedPath
                { path = path }
    in
    Decode.oneOf
        [ whenTagIs "impossibleError"
            (Decode.succeed ImpossibleError)
        , whenTagIs "constructorAlias"
            (Decode.map4 makeConstructorAlias
                decodeCulprit
                (Decode.field "typeName" FQN.decode)
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenTagIs "missingConstructorName"
            (Decode.map2 makeMissingConstructorName
                decodeCulprit
                (Decode.field "typeName" FQN.decode)
            )
        , whenTagIs "nestedDeclAlias"
            (Decode.map3 makeNestedDeclAlias
                decodeCulprit
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenTagIs "strayConstructor"
            (Decode.map2 makeStrayConstructor
                decodeCulprit
                (Decode.field "constructorName" FQN.decode)
            )
        , whenTagIs "libFoundAtUnexpectedPath"
            (Decode.map makeLibFoundAtUnexpectedPath (Decode.field "path" FQN.decode))
        , Decode.succeed UnknownError
        ]


decodeCulprit : Decoder DiffErrorCulprit
decodeCulprit =
    Decode.oneOf
        [ whenFieldIs "oldOrNewBranch" "new" (Decode.succeed SourceBranch)
        , whenFieldIs "oldOrNewBranch" "old" (Decode.succeed TargetBranch)
        ]


decode : Int -> Decoder BranchDiffState
decode numTries =
    Decode.oneOf
        [ whenTagIs "computing" (Decode.succeed (Computing { numTries = numTries }))
        , whenPathIs [ "diff", "tag" ] "ok" (Decode.map Computed BranchDiff.decode)
        , whenPathIs [ "diff", "tag" ]
            "error"
            (Decode.map Uncomputable (Decode.at [ "diff", "error" ] decodeDiffError))
        ]
