type SyntaxSegment = { annotation?: { tag: string }; segment: string };
type DocElement = { annotation?: { tag: string }; segment: string };

type DefinitionSyntax = {
  contents: Array<SyntaxSegment>;
  tag: "UserObject" | "BuiltinObject";
};

type APITerm = {
  bestTermName: string;
  defnTermTag: string;
  signature: Array<SyntaxSegment>;
  termDefinition: DefinitionSyntax;
  termDocs: Array<DocElement>;
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
  DocElement,
};
