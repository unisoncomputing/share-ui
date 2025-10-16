module UnisonShare.DefinitionDiffTests exposing (..)

import Expect
import Json.Decode as Decode
import Test exposing (..)
import UnisonShare.DefinitionDiff as DefinitionDiff


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
