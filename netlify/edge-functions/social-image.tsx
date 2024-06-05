import type { Context } from "@netlify/edge-functions";
import { ImageResponse } from "https://deno.land/x/og_edge/mod.ts";
import React from "https://esm.sh/react@18.2.0";
import * as Route from "./common/share-route.ts";
import { defaultSocialImage } from "./social-image-helpers/social-content.tsx";
import * as Fonts from "./common/fonts.ts";
import userSocialImage from "./social-image-helpers/user-social-image.tsx";
import projectSocialImage from "./social-image-helpers/project-social-image.tsx";

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

export default async (request: Request, _context: Context) => {
  const url = new URL(request.url);
  const path = url.searchParams.get("path") || "/";
  const route = Route.fromPathname(path);

  try {
    return Route.match(route, {
      async UserOverview(handle) {
        console.log("[SocialImage]", "MatchedRoute: UserOverview", handle);
        const content = await userSocialImage(handle);
        const resp = await socialImageResponse(content);

        return resp;
      },

      async ProjectOverview(handle, projectSlug) {
        console.log(
          "[SocialImage]",
          "MatchedRoute: ProjectOverview",
          handle,
          projectSlug
        );
        const content = await projectSocialImage(handle, projectSlug);
        const resp = await socialImageResponse(content);
        return resp;
      },

      async NotFound(url) {
        console.log("[SocialImage]", "Route Not Found", url);
        const resp = await socialImageResponse(await defaultSocialImage());
        return resp;
      },
    });
  } catch (ex) {
    console.error("[SocialImage]", "Error", ex);
    return new Response("Internal Server Error", { status: 500 });
  }
};
