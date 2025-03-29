import { parseISO, format } from "https://esm.sh/date-fns@3.0.0";
import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import { defaultSocialImage } from "./social-content.tsx";
import * as Sizing from "../common/sizing.ts";
import Icon from "./icon.tsx";
import Colors from "../common/colors.ts";
import SocialImageWithSheet from "./social-image-with-sheet.tsx";
import Sheet from "./sheet.tsx";
import Tag from "./tag.tsx";
import { userHandle, hash } from "../common/utils.ts";

async function projectReleaseSocialImage(
  handle: string,
  projectSlug: string,
  version: string,
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectReleaseSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);
  console.log("[ProjectReleaseSocialImage]", "Fetching Release", version);
  const release = await ShareAPI.getRelease(handle, projectSlug, version);

  if (!project) {
    console.log("[ProjectReleaseSocialImage]", "Project not found", projectRef);
    return await defaultSocialImage();
  }

  if (!release) {
    console.log("[ProjectReleaseSocialImage]", "Release not found", version);
    return await defaultSocialImage();
  }

  let latestRelease = null;
  if (version === project.latestRelease) {
    latestRelease = <Tag color="blue" text="Latest Release" />;
  }

  const h =
    typeof release.createdBy === "string"
      ? release.createdBy
      : release.createdBy.handle;

  const author = (
    <div style={STYLES.author}>
      <span style={STYLES.authorHandle}>{userHandle(h)}</span>
      <span style={STYLES.createdAt}>
        {format(parseISO(release.createdAt), "MMM d, y")}
      </span>
    </div>
  );
  const causalHash = (
    <span style={STYLES.hash}>
      <Icon icon="hash" color={Colors.gray.lighten30} size={Sizing.toPx(1.5)} />
      {hash(release.causalHashSquashed)}
    </span>
  );

  return (
    <SocialImageWithSheet>
      <Sheet
        title={`🚀 Release: ${version}`}
        topRowLeft={[projectRef, causalHash]}
        topRowRight={latestRelease}
        footer={author}
      />
    </SocialImageWithSheet>
  );
}

const STYLES = {
  branches: {
    display: "flex",
    alignItems: "center",
    gap: Sizing.toPx(0.5),
  },
  branch: {
    color: Colors.gray.base,
  },
  hash: {
    display: "flex",
    alignItems: "center",
    gap: Sizing.toPx(0),
    fontSize: Sizing.toPx(1.5),
  },
  author: {
    display: "flex",
    flexDirection: "column",
    gap: Sizing.toPx(0.5),
  },
  authorHandle: {
    color: Colors.gray.base,
  },
  createdAt: {
    color: Colors.gray.lighten20,
  },
};

export default projectReleaseSocialImage;
