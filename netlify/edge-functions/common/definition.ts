type SyntaxSegment = {
  annotation?: { tag: string };
  segment: string;
};

type DocSpecial =
  | { tag: "Source"; contents: Array<SyntaxSegment> }
  | { tag: "Link"; contents: Array<SyntaxSegment> }
  | { tag: "Example"; contents: Array<SyntaxSegment> }
  | { tag: "ExampleBlock"; contents: Array<SyntaxSegment> }
  | { tag: "Signature"; contents: Array<Array<SyntaxSegment>> }
  | { tag: "SignatureInline"; contents: Array<SyntaxSegment> }
  | { tag: "Eval"; contents: [Array<SyntaxSegment>, Array<SyntaxSegment>] }
  | {
      tag: "EvalInline";
      contents: [Array<SyntaxSegment>, Array<SyntaxSegment>];
    }
  | { tag: "Embed"; contents: Array<SyntaxSegment> }
  | { tag: "EmbedInline"; contents: Array<SyntaxSegment> };

type DocElement =
  | { tag: "Word"; contents: string }
  | { tag: "NamedLink"; contents: [DocElement, DocElement] }
  | { tag: "Paragraph"; contents: Array<DocElement> }
  | { tag: "Special"; contents: DocSpecial }
  | { tag: "Span"; contents: Array<DocElement> }
  | { tag: "UntitledSection"; contents: Array<DocElement> }
  | { tag: "Section"; contents: [DocElement, Array<DocElement>] }
  | { tag: "BulletedList"; contents: Array<DocElement> }
  | { tag: "NumberedList"; contents: [Number, Array<DocElement>] }
  | { tag: "Code"; contents: DocElement }
  | { tag: "CodeBlock"; contents: [string, DocElement] }
  | { tag: "Group"; contents: DocElement }
  | { tag: "Join"; contents: Array<DocElement> }
  | { tag: "Column"; contents: Array<DocElement> }
  | { tag: "Image"; contents: [DocElement, DocElement, DocElement?] }
  | { tag: "Folded"; contents: [DocElement, DocElement] }
  | { tag: "Callout"; contents: [DocElement | null, DocElement] }
  | { tag: "Aside"; contents: DocElement }
  | { tag: "Tooltip"; contents: [DocElement, DocElement] }
  | { tag: "SectionBreak"; contents: [] }
  | { tag: "Blankline"; contents: [] }
  | { tag: "Anchor"; contents: [string, DocElement] }
  | { tag: "Style"; contents: [string, DocElement] }
  | { tag: "Blockqoute"; contents: DocElement }
  | { tag: "Italic"; contents: DocElement }
  | { tag: "Bold"; contents: DocElement }
  | { tag: "Strikethrough"; contents: DocElement }
  | { tag: "Table"; contents: Array<Array<DocElement>> };

type DefinitionSyntax = {
  contents: Array<SyntaxSegment>;
  tag: "UserObject" | "BuiltinObject";
};

type APITerm = {
  bestTermName: string;
  defnTermTag: string;
  signature: Array<SyntaxSegment>;
  termDefinition: DefinitionSyntax;
  termDocs: Array<[string, string, DocElement]>;
};

type APIType = {
  bestTypeName: string;
  defnTypeTag: string;
  typeDefinition: DefinitionSyntax;
  typeDocs: Array<DocElement>;
};

type APIDefinitions = {
  missingDefinitions: Array<string>;
  termDefinitions: Array<{ [key: string]: APITerm }>;
  typeDefinitions: Array<{ [key: string]: APIType }>;
};

export {
  SyntaxSegment,
  DefinitionSyntax,
  APITerm,
  APIType,
  APIDefinitions,
  DocSpecial,
  DocElement,
};
