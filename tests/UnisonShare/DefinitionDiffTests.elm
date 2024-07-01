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
    "contents": [
      {
        "diffTag": "old",
        "elements": [
          {
            "annotation": {
              "contents": "beforeTerm",
              "tag": "HashQualifier"
            },
            "segment": "beforeTerm"
          }
        ]
      },
      {
        "diffTag": "new",
        "elements": [
          {
            "annotation": {
              "contents": "afterTerm",
              "tag": "HashQualifier"
            },
            "segment": "afterTerm"
          }
        ]
      },
      {
        "diffTag": "both",
        "elements": [
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
              "contents": "##Nat",
              "tag": "TypeReference"
            },
            "segment": "Nat"
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
              "contents": "##Nat",
              "tag": "TypeReference"
            },
            "segment": "Nat"
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
          }
        ]
      },
      {
        "diffTag": "new",
        "elements": [
          {
            "annotation": {
              "contents": "##Nat",
              "tag": "TypeReference"
            },
            "segment": "Nat"
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
          }
        ]
      },
      {
        "diffTag": "both",
        "elements": [
          {
            "annotation": {
              "tag": "DelayForceChar"
            },
            "segment": "'"
          },
          {
            "annotation": {
              "contents": "##Nat",
              "tag": "TypeReference"
            },
            "segment": "Nat"
          },
          {
            "annotation": null,
            "segment": " "
          }
        ]
      },
      {
        "diffTag": "old",
        "elements": [
          {
            "annotation": {
              "contents": "beforeTerm",
              "tag": "HashQualifier"
            },
            "segment": "beforeTerm"
          }
        ]
      },
      {
        "diffTag": "new",
        "elements": [
          {
            "annotation": {
              "contents": "afterTerm",
              "tag": "HashQualifier"
            },
            "segment": "afterTerm"
          }
        ]
      },
      {
        "diffTag": "both",
        "elements": [
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "x"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "y"
          }
        ]
      },
      {
        "diffTag": "new",
        "elements": [
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "z"
          }
        ]
      },
      {
        "diffTag": "both",
        "elements": [
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
              "tag": "ControlKeyword"
            },
            "segment": "do"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": null,
            "segment": "  "
          },
          {
            "annotation": {
              "tag": "UseKeyword"
            },
            "segment": "use "
          },
          {
            "annotation": {
              "tag": "UsePrefix"
            },
            "segment": "Nat"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "UseSuffix"
            },
            "segment": "+"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": null,
            "segment": "  "
          },
          {
            "annotation": {
              "contents": "myList",
              "tag": "HashQualifier"
            },
            "segment": "myList"
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
              "contents": "##Sequence",
              "tag": "TypeReference"
            },
            "segment": "["
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "x"
          },
          {
            "annotation": {
              "contents": "##Sequence",
              "tag": "TypeReference"
            },
            "segment": ", "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "y"
          },
          {
            "annotation": {
              "contents": "##Sequence",
              "tag": "TypeReference"
            },
            "segment": ", "
          }
        ]
      },
      {
        "diffTag": "annotationChange",
        "fromAnnotation": {
          "contents": "#gmjjuqjosr81oqnm9ck6atmve2tnnuu4c5nk89apmg79nsu5djpl6av0os0ekqil0gekdjoianh1rrem7lddsq2i26itq40tbkfgon0",
          "tag": "TermReference"
        },
        "segment": "valueChangesButNameStaysSame",
        "toAnnotation": {
          "contents": "#gjmq673r1vrurfotlnirv7vutdhm6sa3s02em5g22kk606mv6duvv8be402dv79312i4a0onepq5bo7citsodvq2g720nttj0ee9p0g",
          "tag": "TermReference"
        }
      },
      {
        "diffTag": "both",
        "elements": [
          {
            "annotation": {
              "contents": "##Sequence",
              "tag": "TypeReference"
            },
            "segment": ", "
          }
        ]
      },
      {
        "annotation": {
          "contents": "#pi25gcdv0oq0no6k2ahe6t849u7ht4lopeg5fve58ga5t17a49f1dkbmdm6dn063bn3tsd4adijr4ltf7ad6do8u71oa72i27oack2o",
          "tag": "TermReference"
        },
        "diffTag": "segmentChange",
        "fromSegment": "nameChangesButValueStaysSame",
        "toSegment": "changedNameWithSameValue"
      },
      {
        "diffTag": "both",
        "elements": [
          {
            "annotation": {
              "contents": "##Sequence",
              "tag": "TypeReference"
            },
            "segment": "]"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": null,
            "segment": "  "
          },
          {
            "annotation": {
              "contents": "myList2",
              "tag": "HashQualifier"
            },
            "segment": "myList2"
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
              "contents": "#6frvp5jfjtt7odi9769i0p5phuuuij1fi1d9l5ncpelh416ab3vceaphhaijh0ct0v9n793j7e4h78687oij6ai97085u63m264gj5o",
              "tag": "TermReference"
            },
            "segment": "List.map"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Parenthesis"
            },
            "segment": "("
          },
          {
            "annotation": null,
            "segment": "x"
          },
          {
            "annotation": {
              "tag": "ControlKeyword"
            },
            "segment": " ->"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "x"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "contents": "##Nat.+",
              "tag": "TermReference"
            },
            "segment": "+"
          },
          {
            "annotation": null,
            "segment": " "
          }
        ]
      },
      {
        "diffTag": "old",
        "elements": [
          {
            "annotation": {
              "tag": "NumericLiteral"
            },
            "segment": "1"
          }
        ]
      },
      {
        "diffTag": "new",
        "elements": [
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "z"
          }
        ]
      },
      {
        "diffTag": "both",
        "elements": [
          {
            "annotation": {
              "tag": "Parenthesis"
            },
            "segment": ")"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "myList"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": null,
            "segment": "  "
          },
          {
            "annotation": {
              "contents": "##List.size",
              "tag": "TermReference"
            },
            "segment": "List.size"
          },
          {
            "annotation": null,
            "segment": " "
          },
          {
            "annotation": {
              "tag": "Var"
            },
            "segment": "myList2"
          }
        ]
      }
    ],
    "tag": "UserObject"
  },
  "diffKind": "diff",
  "newBranchRef": "after",
  "newTerm": {
    "bestTermName": "afterTerm",
    "defnTermTag": "Plain",
    "signature": [
      {
        "annotation": {
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
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
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
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
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
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
          "tag": "DelayForceChar"
        },
        "segment": "'"
      },
      {
        "annotation": {
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
      }
    ],
    "termDefinition": {
      "contents": [
        {
          "annotation": {
            "contents": "afterTerm",
            "tag": "HashQualifier"
          },
          "segment": "afterTerm"
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
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
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
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
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
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
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
            "tag": "DelayForceChar"
          },
          "segment": "'"
        },
        {
          "annotation": {
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "afterTerm",
            "tag": "HashQualifier"
          },
          "segment": "afterTerm"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "y"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "z"
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
            "tag": "ControlKeyword"
          },
          "segment": "do"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "tag": "UseKeyword"
          },
          "segment": "use "
        },
        {
          "annotation": {
            "tag": "UsePrefix"
          },
          "segment": "Nat"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "UseSuffix"
          },
          "segment": "+"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "myList",
            "tag": "HashQualifier"
          },
          "segment": "myList"
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
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": "["
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "y"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "contents": "#gjmq673r1vrurfotlnirv7vutdhm6sa3s02em5g22kk606mv6duvv8be402dv79312i4a0onepq5bo7citsodvq2g720nttj0ee9p0g",
            "tag": "TermReference"
          },
          "segment": "valueChangesButNameStaysSame"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "contents": "#pi25gcdv0oq0no6k2ahe6t849u7ht4lopeg5fve58ga5t17a49f1dkbmdm6dn063bn3tsd4adijr4ltf7ad6do8u71oa72i27oack2o",
            "tag": "TermReference"
          },
          "segment": "changedNameWithSameValue"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": "]"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "myList2",
            "tag": "HashQualifier"
          },
          "segment": "myList2"
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
            "contents": "#6frvp5jfjtt7odi9769i0p5phuuuij1fi1d9l5ncpelh416ab3vceaphhaijh0ct0v9n793j7e4h78687oij6ai97085u63m264gj5o",
            "tag": "TermReference"
          },
          "segment": "List.map"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Parenthesis"
          },
          "segment": "("
        },
        {
          "annotation": null,
          "segment": "x"
        },
        {
          "annotation": {
            "tag": "ControlKeyword"
          },
          "segment": " ->"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "##Nat.+",
            "tag": "TermReference"
          },
          "segment": "+"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "z"
        },
        {
          "annotation": {
            "tag": "Parenthesis"
          },
          "segment": ")"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "myList"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "##List.size",
            "tag": "TermReference"
          },
          "segment": "List.size"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "myList2"
        }
      ],
      "tag": "UserObject"
    },
    "termDocs": [],
    "termNames": [
      "afterTerm"
    ]
  },
  "oldBranchRef": "before",
  "oldTerm": {
    "bestTermName": "beforeTerm",
    "defnTermTag": "Plain",
    "signature": [
      {
        "annotation": {
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
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
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
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
          "tag": "DelayForceChar"
        },
        "segment": "'"
      },
      {
        "annotation": {
          "contents": "##Nat",
          "tag": "TypeReference"
        },
        "segment": "Nat"
      }
    ],
    "termDefinition": {
      "contents": [
        {
          "annotation": {
            "contents": "beforeTerm",
            "tag": "HashQualifier"
          },
          "segment": "beforeTerm"
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
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
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
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
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
            "tag": "DelayForceChar"
          },
          "segment": "'"
        },
        {
          "annotation": {
            "contents": "##Nat",
            "tag": "TypeReference"
          },
          "segment": "Nat"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "beforeTerm",
            "tag": "HashQualifier"
          },
          "segment": "beforeTerm"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "y"
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
            "tag": "ControlKeyword"
          },
          "segment": "do"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "tag": "UseKeyword"
          },
          "segment": "use "
        },
        {
          "annotation": {
            "tag": "UsePrefix"
          },
          "segment": "Nat"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "UseSuffix"
          },
          "segment": "+"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "myList",
            "tag": "HashQualifier"
          },
          "segment": "myList"
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
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": "["
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "y"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "contents": "#gmjjuqjosr81oqnm9ck6atmve2tnnuu4c5nk89apmg79nsu5djpl6av0os0ekqil0gekdjoianh1rrem7lddsq2i26itq40tbkfgon0",
            "tag": "TermReference"
          },
          "segment": "valueChangesButNameStaysSame"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": ", "
        },
        {
          "annotation": {
            "contents": "#pi25gcdv0oq0no6k2ahe6t849u7ht4lopeg5fve58ga5t17a49f1dkbmdm6dn063bn3tsd4adijr4ltf7ad6do8u71oa72i27oack2o",
            "tag": "TermReference"
          },
          "segment": "nameChangesButValueStaysSame"
        },
        {
          "annotation": {
            "contents": "##Sequence",
            "tag": "TypeReference"
          },
          "segment": "]"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "myList2",
            "tag": "HashQualifier"
          },
          "segment": "myList2"
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
            "contents": "#6frvp5jfjtt7odi9769i0p5phuuuij1fi1d9l5ncpelh416ab3vceaphhaijh0ct0v9n793j7e4h78687oij6ai97085u63m264gj5o",
            "tag": "TermReference"
          },
          "segment": "List.map"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Parenthesis"
          },
          "segment": "("
        },
        {
          "annotation": null,
          "segment": "x"
        },
        {
          "annotation": {
            "tag": "ControlKeyword"
          },
          "segment": " ->"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "x"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "contents": "##Nat.+",
            "tag": "TermReference"
          },
          "segment": "+"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "NumericLiteral"
          },
          "segment": "1"
        },
        {
          "annotation": {
            "tag": "Parenthesis"
          },
          "segment": ")"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "myList"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": null,
          "segment": "  "
        },
        {
          "annotation": {
            "contents": "##List.size",
            "tag": "TermReference"
          },
          "segment": "List.size"
        },
        {
          "annotation": null,
          "segment": " "
        },
        {
          "annotation": {
            "tag": "Var"
          },
          "segment": "myList2"
        }
      ],
      "tag": "UserObject"
    },
    "termDocs": [],
    "termNames": [
      "beforeTerm"
    ]
  },
  "project": "@transcripts/definition-diff"
}
"""


mismatchedJson : String
mismatchedJson =
    """ 
{
  "diffKind": "mismatched",
  "newBranchRef": "after",
  "newTerm": {
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
  "oldTerm": {
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
