import { parseISO, format } from "https://esm.sh/date-fns@3.0.0";
import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import { defaultSocialImage } from "./social-content.tsx";
import * as Sizing from "../common/sizing.ts";
import Colors from "../common/colors.ts";
import Icon from "./icon.tsx";
import SocialImageWithLargeSheet from "./social-image-with-large-sheet.tsx";
import Sheet from "./sheet.tsx";
import { Tag, TagColor } from "./tag.tsx";
import { titleize, userHandle, truncate } from "../common/utils.ts";

async function projectContributionSocialImage(
  handle: string,
  projectSlug: string,
  contribRef: number
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log(
    "[ProjectContributionSocialImage]",
    "Fetching project",
    projectRef
  );
  const project = await ShareAPI.getProject(handle, projectSlug);
  console.log(
    "[ProjectContributionSocialImage]",
    "Fetching contribution",
    contribRef
  );
  const contrib = await ShareAPI.getContribution(
    handle,
    projectSlug,
    contribRef
  );

  if (!project) {
    console.log(
      "[ProjectContributionSocialImage]",
      "Project not found",
      projectRef
    );
    return await defaultSocialImage();
  }

  if (!contrib) {
    console.log(
      "[ProjectContributionSocialImage]",
      "Contribution not found",
      contribRef
    );
    return await defaultSocialImage();
  }

  let statusTagColor: TagColor = "gray";
  let status = titleize(contrib.status);

  if (contrib.status === "in_review") {
    statusTagColor = "blue";
    status = "In Review";
  } else if (contrib.status === "merged") {
    statusTagColor = "purple";
  } else if (contrib.status === "closed") {
    statusTagColor = "orange";
    status = "Archived";
  }

  const sourceBranch = (
    <span style={STYLES.branch}>{contrib.sourceBranchRef}</span>
  );

  const targetBranch = (
    <span style={STYLES.branch}>{contrib.targetBranchRef}</span>
  );

  let comments = null;
  if (contrib.numComments) {
    comments = (
      <span style={STYLES.comments}>
        <Icon
          size={Sizing.toPx(2)}
          color={Colors.gray.lighten30}
          icon="conversation"
        />
        <span>{contrib.numComments}</span>
      </span>
    );
  }

  const author = (
    <div style={STYLES.author}>
      <span style={STYLES.authorHandle}>{userHandle(contrib.author)}</span>
      <span style={STYLES.createdAt}>
        {format(parseISO(contrib.createdAt), "MMM d, y")}
      </span>
    </div>
  );

  return (
    <SocialImageWithLargeSheet>
      <Sheet
        title={truncate(86, contrib.title)}
        topRowLeft={[projectRef, `Contribution #${contribRef}`, comments]}
        topRowRight={<Tag color={statusTagColor} text={status} />}
        bottomRowLeft={
          <span style={STYLES.branches}>
            From {sourceBranch} to {targetBranch}
          </span>
        }
        footer={author}
      />
    </SocialImageWithLargeSheet>
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
  comments: {
    display: "flex",
    alignItems: "center",
    gap: Sizing.toPx(0.5),
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

export default projectContributionSocialImage;
