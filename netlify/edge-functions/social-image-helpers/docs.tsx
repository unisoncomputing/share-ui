/*
Due to this bug: https://github.com/vercel/satori/issues/484
We just render the doc mostly on a single line and, hide the overflow with a fade.
*/
import React from "https://esm.sh/react@18.2.0";
import { DocSpecial, DocElement } from "../common/definition.ts";
import * as Sizing from "../common/sizing.ts";
import Colors from "../common/colors.ts";
import { intersperse } from "../common/utils.ts";
import { InlineSyntax, Syntax } from "./syntax.tsx";
import * as Fonts from "../common/fonts.ts";

function Docs(props: { docRoot: DocElement }) {
  function renderSectionContent(e: DocElement) {
    switch (e.tag) {
      case "Span":
        return <p style={STYLES.docBlock}>{go(e)}</p>;
      case "Paragraph":
        return <p style={STYLES.docBlock}>{go(e)}</p>;
      default:
        return go(e);
    }
  }

  function go(e: DocElement) {
    switch (e.tag) {
      case "Special":
        const special = e.contents;
        switch (special.tag) {
          case "Source":
            return (
              <span style={STYLES.docCode}>
                <Syntax syntax={special.contents} />
              </span>
            );
          case "Link":
            return (
              <span style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
                <InlineSyntax syntax={special.contents} />
              </span>
            );
          case "Example":
            return (
              <span style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
                <InlineSyntax syntax={special.contents} />
              </span>
            );
          case "ExampleBlock":
            return (
              <span style={STYLES.docCode}>
                <Syntax syntax={special.contents} />
              </span>
            );
          case "EmbedInline":
            return (
              <span style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
                <InlineSyntax syntax={special.contents} />
              </span>
            );
          case "Embed":
            return (
              <span style={STYLES.docCode}>
                <Syntax syntax={special.contents} />
              </span>
            );
          case "SignatureInline":
            return (
              <span style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
                <InlineSyntax syntax={special.contents} />
              </span>
            );
          case "Signature":
            return (
              <span style={STYLES.docCode}>
                {special.contents.map((s) => (
                  <Syntax syntax={s} />
                ))}
              </span>
            );
          case "Eval":
            return (
              <span style={{ ...STYLES.docCode }}>
                <Syntax syntax={special.contents[0]} />
                <Syntax syntax={special.contents[1]} />
              </span>
            );
          case "EvalInline":
            return (
              <span style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
                <InlineSyntax syntax={special.contents[0]} />
                <InlineSyntax syntax={special.contents[1]} />
              </span>
            );

          default:
            return <></>;
        }
      case "NamedLink":
        return <a style={STYLES.docLink}>{go(e.contents[0])}</a>;
      case "Word":
        return <span>{e.contents}</span>;
      case "Paragraph":
        return (
          <span style={STYLES.docInline}>
            {intersperse(
              e.contents.map(go),
              <span style={STYLES.docSpace}> </span>
            )}
          </span>
        );
      case "Span":
        return (
          <span style={STYLES.docInline}>
            {intersperse(
              e.contents.map(go),
              <span style={STYLES.docSpace}> </span>
            )}
          </span>
        );
      case "UntitledSection":
        return (
          <section style={STYLES.docBlock}>
            {e.contents.map(renderSectionContent)}
          </section>
        );
      case "Section":
        const [title, sectionContent] = e.contents as [
          DocElement,
          Array<DocElement>
        ];

        return (
          <section style={STYLES.docBlock}>
            <h1 style={STYLES.docBlock}>{go(title)}</h1>
            <div style={STYLES.docBlock}>
              {sectionContent.map(renderSectionContent)}
            </div>
          </section>
        );
      case "BulletedList":
        return (
          <ul style={STYLES.docList}>
            {e.contents.map((e) => (
              <li style={STYLES.docListItem}>
                <span style={STYLES.docListItemBullet}>•</span> {go(e)}
              </li>
            ))}
          </ul>
        );
      case "NumberedList":
        const [start, els] = e.contents;
        return (
          <ol start={start} style={STYLES.docList}>
            {els.map((e, i) => (
              <li style={STYLES.docListItem}>
                <span style={STYLES.docListItemBullet}>{start + i}</span>{" "}
                {go(e)}
              </li>
            ))}
          </ol>
        );
      case "Code":
        return (
          <code style={{ ...STYLES.docCode, ...STYLES.docCodeInline }}>
            {go(e.contents)}
          </code>
        );
      case "Join":
        return <span style={STYLES.docInline}>{e.contents}</span>;
      case "Group":
        return <span style={STYLES.docInline}>{go(e.contents)}</span>;
      default:
        return <></>;
    }
  }

  const docs = go(props.docRoot);

  return (
    <section style={STYLES.docs}>
      {docs}
      <span style={STYLES.docsFade}></span>
    </section>
  );
}

const STYLES = {
  docs: {
    display: "flex",
    paddingTop: Sizing.toPx(2),
    height: Sizing.toPx(5),
    maxHeight: Sizing.toPx(5),
    overflow: "hidden",
    borderTop: `2px solid ${Colors.gray.lighten40}`,
    fontSize: Sizing.toPx(2.25),
    gap: 0,
    position: "relative",
    fontWeight: Fonts.Weights.semiBold,
    border: "1px solid blue",
  },
  docsFade: {
    position: "absolute",
    top: -10,
    right: `-${Sizing.toPx(2.5)}px`,
    bottom: -10,
    width: 400,
    height: Sizing.toPx(6),
    background: `linear-gradient(to right, rgba(255, 255, 255, 0), ${Colors.gray.lighten100}, ${Colors.gray.lighten100})`,
  },
  docBlock: {
    display: "flex",
    flexWrap: "wrap",
    gap: 0,
    marginTop: 0,
    width: "100%",
  },
  docLink: {
    color: Colors.blue1,
  },
  docList: {
    display: "flex",
    flexWrap: "wrap",
    gap: Sizing.toPx(0.5),
    marginTop: 0,
    marginLeft: Sizing.toPx(1),
    width: "100%",
  },
  docListItem: {
    display: "flex",
    flexWrap: "wrap",
    gap: 0,
    marginTop: 0,
    width: "100%",
    listStyleType: "disc",
  },
  docListItemBullet: {
    marginRight: Sizing.toPx(1),
  },
  docInline: {},
  docSpace: {
    width: "8ch",
  },
  docCode: {
    fontFamily: "monospace",
  },
  docCodeInline: {},
};

export default Docs;
