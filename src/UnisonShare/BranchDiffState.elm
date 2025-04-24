module UnisonShare.BranchDiffState exposing (..)

import Code.FullyQualifiedName as FQN exposing (FQN)
import Http
import Json.Decode as Decode exposing (Decoder)
import Lib.Util exposing (whenFieldIs, whenPathIs)
import UnisonShare.BranchDiff as BranchDiff exposing (BranchDiff)


type DiffErrorCulprit
    = TargetBranch
    | SourceBranch


type DiffError
    = ImpossibleError
    | ConstructorAlias
        { typeName : FQN
        , constructorName1 : FQN
        , constructorName2 : FQN
        }
    | MissingConstructorName { typeName : FQN }
    | NestedDeclAlias
        { constructorName1 : FQN
        , constructorName2 : FQN
        }
    | StrayConstructor { constructorName : FQN }
    | UnknownError


type BranchDiffState
    = Loading
    | Computing { numTries : Int }
    | Reloading { numtries : Int }
    | Computed BranchDiff
    | Uncomputable
        { culprit : DiffErrorCulprit
        , error : DiffError
        }
    | Failure Http.Error



-- DECODE


whenErrorIs : String -> Decode.Decoder a -> Decode.Decoder a
whenErrorIs val =
    whenFieldIs "errorKind" val


whenDiffIs : String -> Decode.Decoder a -> Decode.Decoder a
whenDiffIs val =
    whenFieldIs "diffKind" val


decodeDiffError : Decoder DiffError
decodeDiffError =
    let
        makeConstructorAlias typeName ctor1 ctor2 =
            ConstructorAlias
                { typeName = typeName
                , constructorName1 = ctor1
                , constructorName2 = ctor2
                }

        makeMissingConstructorName typeName =
            MissingConstructorName
                { typeName = typeName }

        makeNestedDeclAlias ctor1 ctor2 =
            NestedDeclAlias
                { constructorName1 = ctor1
                , constructorName2 = ctor2
                }

        makeStrayConstructor ctor =
            StrayConstructor
                { constructorName = ctor
                }
    in
    Decode.oneOf
        [ whenErrorIs "impossibleError" (Decode.succeed ImpossibleError)
        , whenErrorIs "constructorAlias"
            (Decode.map3 makeConstructorAlias
                (Decode.field "typeName" FQN.decode)
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenErrorIs "missingConstructorName"
            (Decode.map makeMissingConstructorName
                (Decode.field "typeName" FQN.decode)
            )
        , whenErrorIs "nestedDeclAlias"
            (Decode.map2 makeNestedDeclAlias
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenErrorIs "strayConstructor"
            (Decode.map makeStrayConstructor
                (Decode.field "constructorName" FQN.decode)
            )
        , Decode.succeed UnknownError
        ]


decodeCulprit : Decoder DiffErrorCulprit
decodeCulprit =
    Decode.oneOf
        [ whenPathIs [ "error", "isOldOrNewBranch" ] "new" (Decode.succeed SourceBranch)
        , whenPathIs [ "error", "isOldOrNewBranch" ] "old" (Decode.succeed TargetBranch)
        ]


decode : Int -> Decoder BranchDiffState
decode numTries =
    let
        makeUncomputable culprit error =
            Uncomputable { culprit = culprit, error = error }
    in
    Decode.oneOf
        [ whenDiffIs "computing" (Decode.succeed (Computing { numTries = numTries }))
        , whenDiffIs "ok" (Decode.map Computed BranchDiff.decode)
        , whenDiffIs "error" (Decode.map2 makeUncomputable decodeCulprit (Decode.field "error" decodeDiffError))
        ]
