import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import { MaxLength, defaultSocialImage } from "./social-content.tsx";
import { truncate } from "../common/utils.ts";
import * as Sizing from "../common/sizing.ts";
import SocialImageWithIcon from "./social-image-with-icon.tsx";

async function projectCodeSocialImage(
  handle: string,
  projectSlug: string,
  branchRef?: string,
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectCodeSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);

  if (!project) {
    console.log("[ProjectCodeSocialImage]", "Project not found", projectRef);
    return await defaultSocialImage();
  }

  let summary = projectRef;

  if (branchRef) {
    summary = `${projectRef}/${branchRef}`;

    if (branchRef.length > MaxLength.at3) {
      summary = truncate(MaxLength.at3, branchRef);
    } else if (summary.length > MaxLength.at3) {
      summary = branchRef;
    }
  } else {
    if (projectSlug.length > MaxLength.at3) {
      summary = truncate(MaxLength.at3, projectSlug);
    } else if (summary.length > MaxLength.at3) {
      summary = projectSlug;
    }
  }

  return (
    <SocialImageWithIcon
      title="Code"
      summary={summary}
      summaryFontSize={Sizing.toPx(3)}
      icon="documentCode"
    />
  );
}

export default projectCodeSocialImage;
