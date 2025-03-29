import React from "https://esm.sh/react@18.2.0";
import { ShareAPI, SyntaxSegment } from "../common/share-api.ts";
import { defaultSocialImage } from "./social-content.tsx";
import * as Sizing from "../common/sizing.ts";
import { Icon, IconType } from "./icon.tsx";
import Colors from "../common/colors.ts";
import SocialImageWithSheet from "./social-image-with-sheet.tsx";
import Sheet from "./sheet.tsx";
import { titleize, hash } from "../common/utils.ts";
import Tag from "./tag.tsx";

function Syntax(props: { syntax: Array<SyntaxSegment> }) {
  const segments = props.syntax.map((seg) => {
    const annotationStyle = STYLES[seg.annotation?.tag] || {};
    const style = {
      ...STYLES.segment,
      ...annotationStyle,
    };
    return <span style={style}>{seg.segment}</span>;
  });

  return (
    <pre style={STYLES.syntax}>
      <code>{segments}</code>
      <span style={STYLES.syntaxFade}></span>
    </pre>
  );
}

async function projectDefinitionSocialImage(
  handle: string,
  projectSlug: string,
  branchRef: string,
  definitionType: string,
  ref: Array<string>,
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectDefinitionSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);
  console.log("[ProjectDefinitionSocialImage]", "Fetching Definition", ref);
  const definitions = await ShareAPI.getDefinition(
    handle,
    projectSlug,
    branchRef,
    ref,
  );

  if (!project) {
    console.log(
      "[ProjectDefinitionSocialImage]",
      "Project not found",
      projectRef,
    );
    return await defaultSocialImage();
  }

  let definition = null;

  if (definitionType === "term") {
    const [hash] = Object.keys(definitions.termDefinitions);
    const raw = definitions.termDefinitions[hash];
    if (hash) {
      definition = {
        hash,
        name: raw.bestTermName,
        category: raw.defnTermeTag,
        signature: raw.signature,
        definition: raw.termDefinition,
      };
    }
  } else {
    const [hash] = Object.keys(definitions.typeDefinitions);
    const raw = definitions.typeDefinitions[hash];
    if (hash) {
      definition = {
        hash,
        name: raw.bestTypeName,
        category: raw.defnTypeTag,
        definition: raw.typeDefinition,
      };
    }
  }

  if (!definition) {
    console.log("[ProjectDefinitionSocialImage]", "Definition not found", ref);
    return await defaultSocialImage();
  }

  const branchRef_ = (
    <span style={STYLES.branch}>
      <Icon icon="branch" size={Sizing.toPx(2)} color={Colors.gray.lighten30} />
      {branchRef}
    </span>
  );

  let definitionIcon: IconType;
  let syntax: React.Element;
  if (definitionType === "term") {
    definitionIcon = "term";
    if (definition.definition.tag === "UserObject") {
      syntax = <Syntax syntax={definition.signature} />;
    } else {
      syntax = (
        <span style={STYLES.signatureAndBuiltin}>
          <Syntax syntax={definition.signature} />
          <Tag
            color="gray"
            text={`${definition.name} is a built-in term provided by the Unison runtime`}
          />
        </span>
      );
    }
  } else {
    definitionIcon = "type";

    if (definition.definition.tag === "UserObject") {
      syntax = <Syntax syntax={definition.definition.contents} />;
    } else {
      syntax = (
        <Tag
          color="gray"
          text={`${definition.name} is a built-in type provided by the Unison runtime`}
        />
      );
    }
  }

  const definitionType_ = (
    <span style={STYLES.definitionType}>
      <Icon
        icon={definitionIcon}
        size={Sizing.toPx(1.5)}
        color={Colors.gray.lighten30}
      />
      {titleize(definitionType)}
    </span>
  );

  const definitionHash = (
    <span style={STYLES.hash}>
      <Icon icon="hash" color={Colors.gray.lighten30} size={Sizing.toPx(1.5)} />
      {hash(definition.hash)}
    </span>
  );

  let topRowRight = [definitionType_, definitionHash];

  if (projectRef.length + branchRef.length > 40) {
    topRowRight = [];
  }

  return (
    <SocialImageWithSheet>
      <Sheet
        title={definition.name}
        topRowLeft={[projectRef, branchRef_]}
        topRowRight={topRowRight}
        bottomRowLeft={syntax}
      />
    </SocialImageWithSheet>
  );
}

const STYLES = {
  branch: {
    display: "flex",
    alignItems: "center",
    gap: Sizing.toPx(0.25),
  },
  definitionType: {
    display: "flex",
    alignItems: "center",
    fontSize: Sizing.toPx(1.5),
    gap: Sizing.toPx(0.25),
  },
  hash: {
    display: "flex",
    alignItems: "center",
    gap: Sizing.toPx(0),
    fontSize: Sizing.toPx(1.5),
  },
  signatureAndBuiltin: {
    display: "flex",
    flexDirection: "column",
    gap: Sizing.toPx(0.5),
  },
  syntax: {
    position: "relative",
    margin: 0,
    padding: 0,
    fontSize: Sizing.toPx(2),
    lineHeight: 1.2,
    width: "100%",
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

export default projectDefinitionSocialImage;
