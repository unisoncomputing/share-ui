import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import { MaxLength, defaultSocialImage } from "./social-content.tsx";
import { truncate } from "../common/utils.ts";
import * as Sizing from "../common/sizing.ts";
import SocialImageWithIcon from "./social-image-with-icon.tsx";

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
  if (summary && summary.length > MaxLength.at3 * 1.5) {
    summaryFontSize = Sizing.toPx(2.5);
  }

  return (
    <SocialImageWithIcon
      title={title}
      titleFontSize={titleFontSize}
      summary={summary}
      summaryFontSize={summaryFontSize}
      icon="pencilRuler"
    />
  );
}

export default projectSocialImage;
