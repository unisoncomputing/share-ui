import type { Context } from "@netlify/edge-functions";
import { ImageResponse } from "https://deno.land/x/og_edge/mod.ts";
import React from "https://esm.sh/react@18.2.0";
import * as Route from "./common/share-route.ts";
import { defaultSocialImage } from "./social-image-helpers/social-content.tsx";
import * as Fonts from "./common/fonts.ts";
import userSocialImage from "./social-image-helpers/user-social-image.tsx";
import projectSocialImage from "./social-image-helpers/project-social-image.tsx";
import projectTicketsSocialImage from "./social-image-helpers/project-tickets-social-image.tsx";
import projectContributionsSocialImage from "./social-image-helpers/project-contributions-social-image.tsx";
import projectCodeSocialImage from "./social-image-helpers/project-code-social-image.tsx";
import projectReleasesSocialImage from "./social-image-helpers/project-releases-social-image.tsx";
import projectBranchesSocialImage from "./social-image-helpers/project-branches-social-image.tsx";

async function socialImageResponse(
  content: React.Element
): Promise<ImageResponse> {
  const fonts = await Fonts.load();

  return new ImageResponse(content, {
    width: 1200,
    height: 630,
    fonts: fonts,
  });
}

function generateSocialImage(url: URL) {
  const path = (url.searchParams.get("path") || "/").replaceAll('"', "");
  const route = Route.fromPathname(path);

  console.log("URL Route matching", path, route);

  return route.caseOf({
    async UserOverview(handle) {
      console.log("MatchedRoute: UserOverview", handle);
      const content = await userSocialImage(handle);
      const resp = await socialImageResponse(content);

      return resp;
    },

    async ProjectOverview(handle, projectSlug) {
      console.log("MatchedRoute: ProjectOverview", handle, projectSlug);
      const content = await projectSocialImage(handle, projectSlug);
      const resp = await socialImageResponse(content);
      return resp;
    },

    async ProjectCode(handle, projectSlug, branchRef) {
      console.log("MatchedRoute: ProjectCode", handle, projectSlug);
      const content = await projectCodeSocialImage(
        handle,
        projectSlug,
        branchRef
      );
      const resp = await socialImageResponse(content);
      return resp;
    },

    async ProjectTickets(handle, projectSlug) {
      console.log("MatchedRoute: ProjectTickets", handle, projectSlug);
      const content = await projectTicketsSocialImage(handle, projectSlug);
      const resp = await socialImageResponse(content);
      return resp;
    },

    async ProjectContributions(handle, projectSlug) {
      console.log("MatchedRoute: ProjectContributions", handle, projectSlug);
      const content = await projectContributionsSocialImage(
        handle,
        projectSlug
      );
      const resp = await socialImageResponse(content);
      return resp;
    },

    async ProjectReleases(handle, projectSlug) {
      console.log("MatchedRoute: ProjectReleases", handle, projectSlug);
      const content = await projectReleasesSocialImage(handle, projectSlug);
      const resp = await socialImageResponse(content);
      return resp;
    },

    async ProjectBranches(handle, projectSlug) {
      console.log("MatchedRoute: ProjectBranches", handle, projectSlug);
      const content = await projectBranchesSocialImage(handle, projectSlug);
      const resp = await socialImageResponse(content);
      return resp;
    },

    async NotFound(url) {
      console.log("Route Not Found", url);
      const resp = await socialImageResponse(await defaultSocialImage());
      return resp;
    },
  });
}

export default async (request: Request, _context: Context) => {
  try {
    const resp = await generateSocialImage(new URL(request.url));
    console.log("got resp", resp);
    return resp;
  } catch (ex) {
    console.error("Error", ex);
    return new Response("Internal Server Error", { status: 500 });
  }
};
