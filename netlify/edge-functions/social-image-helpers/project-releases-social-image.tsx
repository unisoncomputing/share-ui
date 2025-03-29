import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import { MaxLength, defaultSocialImage } from "./social-content.tsx";
import { truncate } from "../common/utils.ts";
import * as Sizing from "../common/sizing.ts";
import SocialImageWithIcon from "./social-image-with-icon.tsx";

async function projectReleasesSocialImage(
  handle: string,
  projectSlug: string,
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectReleasesSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);

  if (!project) {
    console.log(
      "[ProjectReleasesSocialImage]",
      "Project not found",
      projectRef,
    );
    return await defaultSocialImage();
  }

  let summary = projectRef;

  if (projectSlug.length > MaxLength.at3) {
    summary = truncate(MaxLength.at3, projectSlug);
  } else if (summary.length > MaxLength.at3) {
    summary = projectSlug;
  }

  return (
    <SocialImageWithIcon
      title="Releases"
      summary={summary}
      summaryFontSize={Sizing.toPx(3)}
      icon="rocket"
    />
  );
}

export default projectReleasesSocialImage;
