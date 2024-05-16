import { Context } from "@netlify/edge-functions";
import ShareAPI from "./common/share-api.ts";
import {
  SocialContent,
  DEFAULT_SOCIAL_CONTENT,
} from "./common/social-content.ts";
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
`;

async function getContent(rawUrl: string): Promise<SocialContent> {
  const url = new URL(rawUrl);
  const route = Route.parse(rawUrl);

  try {
    return Route.match(route, {
      async UserOverview(handle) {
        const user = await ShareAPI.getUser(handle);

        if (!user) return DEFAULT_SOCIAL_CONTENT;

        const nameAndHandle = user.name
          ? `${user.name} @${user.handle}` : `@${user.handle}`;

        return {
          title: `${nameAndHandle} | Unison Share`,
          description: user.bio || DEFAULT_SOCIAL_CONTENT.description,
          imageUrl: `${url.protocol}//${
            url.hostname
          }/social-image?path=${encodeURI(url.pathname)}`,
        };
      },

      async ProjectOverview(handle, projectSlug) {
        return DEFAULT_SOCIAL_CONTENT;
        /*
        const project = await ShareAPI.getProject(handle, projectSlug);

        if (!project) return DEFAULT_SOCIAL_CONTENT;

        return {
          title: `${handle}/${projectSlug} | Unison Share`,
          description: project.summary || DEFAULT_SOCIAL_CONTENT.description,
          imageUrl: `${url.protocol}//${
            url.hostname
          }/social-image?path=${encodeURI(url.pathname)}`,
        };
        */
      },

      async NotFound(_) {
        return DEFAULT_SOCIAL_CONTENT;
      },
    });
  } catch (ex) {
    console.error(ex);
    return DEFAULT_SOCIAL_CONTENT;
  }
}

async function replaceSocialContent(
  request: Request,
  context: Context
): Promise<Response> {
  const content = await getContent(request.url);

  const response = await context.next();
  const page = await response.text();

  const regex = /<meta name="social" content="content"\/>/i;
  const newContent = template
    .replaceAll("{{TITLE}}", content.title)
    .replaceAll("{{DESCRIPTION}}", content.description)
    .replaceAll("{{IMAGE_URL}}", content.imageUrl);

  const updatedPage = page.replace(regex, newContent);
  return new Response(updatedPage, response);
}

export default replaceSocialContent;
