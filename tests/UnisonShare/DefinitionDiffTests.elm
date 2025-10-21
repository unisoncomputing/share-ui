module UnisonShare.DefinitionDiffTests exposing (..)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import UnisonShare.DefinitionDiff as DefinitionDiff exposing (Collapsed(..), DiffLine(..))


toCollapsedWithLineNums : Test
toCollapsedWithLineNums =
    describe "DefinitionDiff.toCollapsedWithLineNum"
        [ describe "With ChangeLines in the middle"
            [ test "marks collapsable lines as collapsed if 3 lines on either side of a line is an UnchangedLine and adds line numbers" <|
                \_ ->
                    let
                        input =
                            [ unchanged 0
                            , unchanged 0
                            , unchanged 0
                            , unchanged 0
                            , unchanged 0
                            , changed 0
                            , unchanged 0
                            , unchanged 0
                            , unchanged 0
                            , unchanged 0
                            , unchanged 0
                            ]

                        expected =
                            [ Collapsed [ unchanged 1, unchanged 2 ]
                            , NotCollapsed
                                [ unchanged 3
                                , unchanged 4
                                , unchanged 5
                                , changed 6
                                , unchanged 7
                                , unchanged 8
                                , unchanged 9
                                ]
                            , Collapsed
                                [ unchanged 10
                                , unchanged 11
                                ]
                            ]
                    in
                    Expect.equal expected (DefinitionDiff.toCollapsedWithLineNums input)
            , describe "With ChangedLines in the start and end"
                [ test "marks collapsable lines as collapsed if 3 lines on either side of a line is an UnchangedLine and adds line numbers" <|
                    \_ ->
                        let
                            input =
                                [ changed 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , unchanged 0
                                , changed 0
                                ]

                            expected =
                                [ NotCollapsed
                                    [ changed 1
                                    , unchanged 2
                                    , unchanged 3
                                    , unchanged 4
                                    ]
                                , Collapsed
                                    [ unchanged 5
                                    , unchanged 6
                                    , unchanged 7
                                    , unchanged 8
                                    ]
                                , NotCollapsed
                                    [ unchanged 9
                                    , unchanged 10
                                    , unchanged 11
                                    , changed 12
                                    ]
                                ]
                        in
                        Expect.equal expected (DefinitionDiff.toCollapsedWithLineNums input)
                ]
            ]
        ]


decode : Test
decode =
    describe "DefinitionDiff.decode"
        [ test "decodes a diff of diffKind 'diff'" <|
            \_ ->
                Expect.ok
                    (Decode.decodeString
                        (DefinitionDiff.decode DefinitionDiff.Term)
                        diffJson
                    )
        , test "decodes a diff of diffKind 'mismatched'" <|
            \_ ->
                Expect.ok
                    (Decode.decodeString
                        (DefinitionDiff.decode DefinitionDiff.Term)
                        mismatchedJson
                    )
        ]


unchanged : Int -> DiffLine
unchanged ln =
    UnchangedLine { lineNum = ln, segments = [] }


changed : Int -> DiffLine
changed ln =
    ChangedLine { lineNum = ln, segments = [] }


spacer : DiffLine
spacer =
    Spacer { numLines = 1 }


diffJson : String
diffJson =
    """
{
  "diff": {
    "diffKind": "diff",
    "diff": {
      "contents": {
        "left": [
          {
            "kind": "unchanged",
            "value": [
              {
                "diffTag": "both",
                "elements": [
                  {
                    "annotation": {
                      "contents": "beforeTerm",
                      "tag": "HashQualifier"
                    },
                    "segment": "beforeTerm"
                  }
                ]
              }
            ]
          }
        ],
        "right": [
          {
            "kind": "changed",
            "value": [
              {
                "diffTag": "oneSided",
                "elements": [
                  {
                    "annotation": {
                      "contents": "afterTerm",
                      "tag": "HashQualifier"
                    },
                    "segment": "afterTerm"
                  }
                ]
              }
            ]
          }
        ]
      },
      "tag": "UserObject"
    }
  },
  "newBranchRef": "after",
  "right": {
    "bestTermName": "afterTerm",
    "defnTermTag": "Plain",
    "signature": [],
    "termDefinition": {
      "contents": [],
      "tag": "UserObject"
    },
    "termDocs": [],
    "termNames": ["afterTerm"]
  },
  "oldBranchRef": "before",
  "left": {
    "bestTermName": "beforeTerm",
    "defnTermTag": "Plain",
    "signature": [],
    "termDefinition": {
      "contents": [],
      "tag": "UserObject"
    },
    "termDocs": [],
    "termNames": ["beforeTerm"]
  },
  "project": "@test/definition-diff"
}
"""


mismatchedJson : String
mismatchedJson =
    """
{
  "diff": {
    "diffKind": "mismatched"
  },
  "newBranchRef": "after",
  "right": {
    "bestTermName": "newBug",
    "defnTermTag": "Plain",
    "signature": [
      {
        "annotation": {
          "contents": "##Text",
          "tag": "TypeReference"
        },
        "segment": "Text"
      }
    ],
    "termDefinition": {
      "contents": [
        {
          "annotation": {
            "contents": "newBug",
            "tag": "HashQualifier"
          },
          "segment": "newBug"
        },
        {
          "annotation": {
            "tag": "TypeAscriptionColon"
          },
          "segment": " :"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "##Text",
            "tag": "TypeReference"
          },
          "segment": "Text"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "newBug",
            "tag": "HashQualifier"
          },
          "segment": "newBug"
        },
        {
          "annotation": {
            "tag": "BindingEquals"
          },
          "segment": " ="
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "TextLiteral"
          },
          "segment": "'No longer a builtin'"
        }
      ],
      "tag": "UserObject"
    },
    "termDocs": [],
    "termNames": [ "newBug" ]
  },
  "oldBranchRef": "before",
  "left": {
    "bestTermName": "bug",
    "defnTermTag": "Plain",
    "signature": [
      {
        "annotation": {
          "tag": "Var"
        },
        "segment": "a"
      },
      {
        "annotation": null,
        "segment": " "
      },
      {
        "annotation": {
          "tag": "TypeOperator"
        },
        "segment": "->"
      },
      {
        "annotation": null,
        "segment": " "
      },
      {
        "annotation": {
          "tag": "Var"
        },
        "segment": "b"
      }
    ],
    "termDefinition": {
      "contents": [
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "a"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "TypeOperator"
          },
          "segment": "->"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "b"
        }
      ],
      "tag": "BuiltinObject"
    },
    "termDocs": [],
    "termNames": ["builtin.bug"]
  },
  "project": "@transcripts/definition-diff"
}
  """
