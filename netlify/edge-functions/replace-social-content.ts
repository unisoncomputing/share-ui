import { Context } from "@netlify/edge-functions";
import ShareAPI from "./common/share-api.ts";
import {
  SocialContent,
  DefaultSocialContent,
} from "./social-image-helpers/social-content.tsx";
import * as Route from "./common/share-route.ts";

/*
Replaces content in <head> thats unique for the URL for sharing links on
Social Media. Things like Open Graph image and description.
*/

const template = `
  <title>{{TITLE}}</title>
  <meta name="description" content="{{DESCRIPTION}}" />
  <meta property="og:title" content="{{TITLE}}" />
  <meta property="og:description" content="{{DESCRIPTION}}" />
  <meta property="og:image" content="{{IMAGE_URL}}" />
  <meta property="og:url" content="{{URL}}" />
`;

function urlToImageUrl(url: URL): string {
  return `${url.protocol}//${url.hostname}/social-image?path=${encodeURI(
    url.pathname
  )}`;
}

async function getContent(rawUrl: string): Promise<SocialContent> {
  const url = new URL(rawUrl);
  const route = Route.parse(rawUrl);
  const imageUrl = urlToImageUrl(url);

  return route.caseOf({
    async UserOverview(handle) {
      const user = await ShareAPI.getUser(handle);

      if (!user) return DefaultSocialContent;

      const nameAndHandle = user.name
        ? `${user.name} @${user.handle}`
        : `@${user.handle}`;

      return {
        title: `${nameAndHandle} | Unison Share`,
        description: user.bio || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectOverview(handle, projectSlug) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectCode(handle, projectSlug, branchRef) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      const title = `Branch: ${branchRef}` || "Code";

      return {
        title: `${title} · ${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectDefinition(
      handle,
      projectSlug,
      branchRef,
      definitionType,
      ref
    ) {
      const project = await ShareAPI.getProject(handle, projectSlug);
      const definitions = await ShareAPI.getDefinition(
        handle,
        projectSlug,
        branchRef,
        ref
      );

      if (!project) return DefaultSocialContent;
      if (!definitions) return DefaultSocialContent;

      let definition = null;
      if (definitionType === "term") {
        const [hash] = Object.keys(definitions.termDefinitions);
        const raw = definitions.termDefinitions[hash];
        if (hash) {
          definition = { name: raw.bestTermName };
        }
      } else {
        const [hash] = Object.keys(definitions.typeDefinitions);
        const raw = definitions.typeDefinitions[hash];
        if (hash) {
          definition = { name: raw.bestTypeName };
        }
      }

      if (!definition) return DefaultSocialContent;
      const title = definition.name;

      return {
        title: `${title} · ${handle}/${projectSlug}/${branchRef} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectTickets(handle, projectSlug) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `Tickets · ${handle}/${projectSlug} Tickets | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectTicket(handle, projectSlug, ticketRef) {
      const project = await ShareAPI.getProject(handle, projectSlug);
      const ticket = await ShareAPI.getTicket(handle, projectSlug, ticketRef);

      if (!project || !ticket) return DefaultSocialContent;

      return {
        title: `#${ticketRef}: ${ticket.title} · ${handle}/${projectSlug} | Unison Share`,
        description: ticket.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectContributions(handle, projectSlug) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `Contributions · ${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectContribution(handle, projectSlug, contribRef) {
      const project = await ShareAPI.getProject(handle, projectSlug);
      const contrib = await ShareAPI.getContribution(
        handle,
        projectSlug,
        contribRef
      );

      if (!project || !contrib) return DefaultSocialContent;

      return {
        title: `#${contribRef}: ${contrib.title} · ${handle}/${projectSlug} | Unison Share`,
        description:
          contrib.description ||
          project.summary ||
          DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectReleases(handle, projectSlug) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `Releases · ${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectRelease(handle, projectSlug, version) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `Release ${version} · ${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async ProjectBranches(handle, projectSlug) {
      const project = await ShareAPI.getProject(handle, projectSlug);

      if (!project) return DefaultSocialContent;

      return {
        title: `Branches · ${handle}/${projectSlug} | Unison Share`,
        description: project.summary || DefaultSocialContent.description,
        imageUrl,
        url: rawUrl,
      };
    },

    async NotFound(_) {
      return DefaultSocialContent;
    },
  });
}

async function replaceSocialContent(
  request: Request,
  context: Context
): Promise<Response> {
  if (request.url.includes("social-image")) {
    return;
  }

  let content = DefaultSocialContent;
  try {
    content = await getContent(request.url);
  } catch (ex) {
    console.error(ex);
    content = DefaultSocialContent;
  }

  const response = await context.next();
  const page = await response.text();

  const regex = /<title>Unison Share<\/title>/i;
  const newContent = template
    .replaceAll("{{TITLE}}", content.title)
    .replaceAll("{{DESCRIPTION}}", content.description)
    .replaceAll("{{IMAGE_URL}}", content.imageUrl)
    .replaceAll("{{URL}}", content.url);

  const updatedPage = page.replace(regex, newContent);
  return new Response(updatedPage, response);
}

export default replaceSocialContent;
