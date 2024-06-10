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
import { titleize, truncate, userHandle } from "../common/utils.ts";

async function projectTicketSocialImage(
  handle: string,
  projectSlug: string,
  ticketRef: number
): Promise<React.Element> {
  const projectRef = `${handle}/${projectSlug}`;
  console.log("[ProjectTicketSocialImage]", "Fetching project", projectRef);
  const project = await ShareAPI.getProject(handle, projectSlug);
  console.log("[ProjectTicketSocialImage]", "Fetching Ticket", ticketRef);
  const ticket = await ShareAPI.getTicket(handle, projectSlug, ticketRef);

  if (!project) {
    console.log("[ProjectTicketSocialImage]", "Project not found", projectRef);
    return await defaultSocialImage();
  }

  if (!ticket) {
    console.log("[ProjectTicketSocialImage]", "Ticket not found", ticketRef);
    return await defaultSocialImage();
  }

  let statusTagColor: TagColor = "gray";
  const status = titleize(ticket.status);

  if (ticket.status === "open") {
    statusTagColor = "green";
  }

  let comments = null;
  if (ticket.numComments) {
    comments = (
      <span style={STYLES.comments}>
        <Icon
          size={Sizing.toPx(2)}
          color={Colors.gray.lighten30}
          icon="conversation"
        />
        <span>{ticket.numComments}</span>
      </span>
    );
  }

  const author = (
    <div style={STYLES.author}>
      <span style={STYLES.authorHandle}>{userHandle(ticket.author)}</span>
      <span style={STYLES.createdAt}>
        {format(parseISO(ticket.createdAt), "MMM d, y")}
      </span>
    </div>
  );

  return (
    <SocialImageWithLargeSheet>
      <Sheet
        title={truncate(86, ticket.title)}
        topRowLeft={[projectRef, `Ticket #${ticketRef}`, comments]}
        topRowRight={<Tag color={statusTagColor} text={status} />}
        footer={author}
      />
    </SocialImageWithLargeSheet>
  );
}

const STYLES = {
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

export default projectTicketSocialImage;
