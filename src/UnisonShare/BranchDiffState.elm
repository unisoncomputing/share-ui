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


whenTagIs : String -> Decode.Decoder a -> Decode.Decoder a
whenTagIs val =
    whenFieldIs "tag" val


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
        [ whenTagIs "impossibleError" (Decode.succeed ImpossibleError)
        , whenTagIs "constructorAlias"
            (Decode.map3 makeConstructorAlias
                (Decode.field "typeName" FQN.decode)
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenTagIs "missingConstructorName"
            (Decode.map makeMissingConstructorName
                (Decode.field "typeName" FQN.decode)
            )
        , whenTagIs "nestedDeclAlias"
            (Decode.map2 makeNestedDeclAlias
                (Decode.field "constructorName1" FQN.decode)
                (Decode.field "constructorName2" FQN.decode)
            )
        , whenTagIs "strayConstructor"
            (Decode.map makeStrayConstructor
                (Decode.field "constructorName" FQN.decode)
            )
        , Decode.succeed UnknownError
        ]


decodeCulprit : Decoder DiffErrorCulprit
decodeCulprit =
    Decode.oneOf
        [ whenFieldIs "isOldOrNewBranch" "new" (Decode.succeed SourceBranch)
        , whenFieldIs "isOldOrNewBranch" "old" (Decode.succeed TargetBranch)
        ]


decode : Int -> Decoder BranchDiffState
decode numTries =
    let
        makeUncomputable culprit error =
            Uncomputable { culprit = culprit, error = error }
    in
    Decode.oneOf
        [ whenTagIs "computing" (Decode.succeed (Computing { numTries = numTries }))
        , whenPathIs [ "diff", "tag" ] "ok" (Decode.map Computed BranchDiff.decode)
        , whenPathIs [ "diff", "tag" ]
            "error"
            (Decode.map2 makeUncomputable
                (Decode.at [ "diff", "error" ] decodeCulprit)
                (Decode.at [ "diff", "error" ] decodeDiffError)
            )
        ]
