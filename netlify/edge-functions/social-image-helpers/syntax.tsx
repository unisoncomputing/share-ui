import React from "https://esm.sh/react@18.2.0";
import { SyntaxSegment } from "../common/definition.ts";
import * as Sizing from "../common/sizing.ts";
import Colors from "../common/colors.ts";

function SyntaxSegment(props: { segment: SyntaxSegment }) {
  const annotationStyle = props.segment.annotation?.tag
    ? STYLES[props.segment.annotation?.tag] || {}
    : STYLES.Blank;
  const style = {
    ...STYLES.segment,
    ...annotationStyle,
  };
  return <span style={style}>{props.segment.segment}</span>;
}

function InlineSyntax(props: { syntax: Array<SyntaxSegment> }) {
  const segments = props.syntax.map((seg) => <SyntaxSegment segment={seg} />);

  return (
    <pre style={STYLES.inlineSyntax}>
      <code>{segments}</code>
    </pre>
  );
}

function Syntax(props: { syntax: Array<SyntaxSegment> }) {
  const segments = props.syntax.map((seg) => <SyntaxSegment segment={seg} />);

  return (
    <pre style={STYLES.syntax}>
      <code>{segments}</code>
      <span style={STYLES.syntaxFade}></span>
    </pre>
  );
}

const STYLES = {
  syntax: {
    position: "relative",
    margin: 0,
    padding: 0,
    fontSize: Sizing.toPx(2),
    lineHeight: 1.2,
    border: "1px solid blue",
  },
  inlineSyntax: {
    margin: 0,
    padding: 0,
  },
  syntaxFade: {
    position: "absolute",
    top: -10,
    right: `-${Sizing.toPx(2.5)}px`,
    bottom: -10,
    width: 400,
    height: Sizing.toPx(4),
    background: `linear-gradient(to right, rgba(255, 255, 255, 0), ${Colors.gray.lighten100}, ${Colors.gray.lighten100})`,
  },
  segment: {
    color: Colors.gray.base,
    fontFamily: "FiraCode",
  },
  Blank: {
    color: Colors.gray.lighten30,
  },
  DataTypeKeyword: {
    color: Colors.gray.lighten30,
  },
  TypeAscriptionColon: {
    color: Colors.gray.lighten30,
  },
  DataTypeModifier: {
    color: Colors.gray.lighten30,
  },
  TypeOperator: {
    color: Colors.gray.lighten30,
  },
  AbilityBraces: {
    color: Colors.gray.lighten30,
  },
  DelimiterChar: {
    color: Colors.gray.lighten30,
  },
};

export { Syntax, InlineSyntax, SyntaxSegment };
