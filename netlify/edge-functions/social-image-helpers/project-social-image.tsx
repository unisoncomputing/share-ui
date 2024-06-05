import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import * as Fonts from "../common/fonts.ts";
import Colors from "../common/colors.ts";
import {
  MaxLength,
  SocialImageStyles,
  defaultSocialImage,
} from "./social-content.tsx";
import { truncate } from "../common/utils.ts";
import IconAvatar from "./icon-avatar.tsx";
import * as Sizing from "../common/sizing.ts";

async function projectSocialImage(
  handle: string,
  projectSlug: string
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);

  if (!project) {
    console.log("[ProjectSocialImage]", "Project not found", projectRef);
    return await defaultSocialImage();
  }

  let title = projectRef;
  let titleFontSize = Sizing.toPx(4);

  if (projectSlug.length > MaxLength.at3) {
    title = truncate(MaxLength.at3, projectSlug);
  } else if (title.length > MaxLength.at3) {
    title = projectSlug;
  } else if (title.length > MaxLength.at4) {
    titleFontSize = Sizing.toPx(3);
  }

  const summary = project.summary;
  let summaryFontSize = Sizing.toPx(3);
  if (summary.length > MaxLength.at3 * 1.5) {
    summaryFontSize = Sizing.toPx(2.5);
  }

  return (
    <div style={SocialImageStyles.base}>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          marginTop: Sizing.toPx(6),
          gap: Sizing.toPx(3),
        }}
      >
        <IconAvatar icon="pencil-ruler" />
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            justifyContent: "center",
            gap: Sizing.toPx(1),
          }}
        >
          <h1
            style={{
              color: Colors.gray.darken30,
              fontSize: titleFontSize,
              fontWeight: Fonts.Weights.bold,
              margin: 0,
            }}
          >
            {title}
          </h1>
          <p
            style={{
              color: Colors.gray.lighten20,
              fontSize: summaryFontSize,
              fontWeight: Fonts.Weights.semiBold,
              margin: 0,
              marginLeft: Sizing.toPx(2),
              marginRight: Sizing.toPx(2),
              lineHeight: 1.4,
              textAlign: "center",
              maxWidth: "100%",
            }}
          >
            {summary}
          </p>
        </div>
      </div>
    </div>
  );
}

export default projectSocialImage;
