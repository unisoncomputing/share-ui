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
`;

async function getContent(rawUrl: string): Promise<SocialContent> {
  const url = new URL(rawUrl);
  const route = Route.parse(rawUrl);

  try {
    return Route.match(route, {
      async UserOverview(handle) {
        const user = await ShareAPI.getUser(handle);

        if (!user) return DefaultSocialContent;

        const nameAndHandle = user.name
          ? `${user.name} @${user.handle}`
          : `@${user.handle}`;

        return {
          title: `${nameAndHandle} | Unison Share`,
          description: user.bio || DefaultSocialContent.description,
          imageUrl: `${url.protocol}//${
            url.hostname
          }/social-image?path=${encodeURI(url.pathname)}`,
        };
      },

      async ProjectOverview(handle, projectSlug) {
        const project = await ShareAPI.getProject(handle, projectSlug);

        if (!project) return DefaultSocialContent;

        return {
          title: `${handle}/${projectSlug} | Unison Share`,
          description: project.summary || DefaultSocialContent.description,
          imageUrl: `${url.protocol}//${
            url.hostname
          }/social-image?path=${encodeURI(url.pathname)}`,
        };
      },

      async NotFound(_) {
        return DefaultSocialContent;
      },
    });
  } catch (ex) {
    console.error(ex);
    return DefaultSocialContent;
  }
}

async function replaceSocialContent(
  request: Request,
  context: Context
): Promise<Response> {
  const content = await getContent(request.url);

  const response = await context.next();
  const page = await response.text();

  const regex = /<title>Unison Share<\/title>/i;
  const newContent = template
    .replaceAll("{{TITLE}}", content.title)
    .replaceAll("{{DESCRIPTION}}", content.description)
    .replaceAll("{{IMAGE_URL}}", content.imageUrl);

  const updatedPage = page.replace(regex, newContent);
  return new Response(updatedPage, response);
}

export default replaceSocialContent;
