import type { Context } from "@netlify/edge-functions";
import { ImageResponse } from "https://deno.land/x/og_edge/mod.ts";
import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "./common/share-api.ts";
import * as Route from "./common/share-route.ts";
import { DEFAULT_SOCIAL_CONTENT } from "./common/social-content.ts";
import Colors from "./common/colors.ts";
import * as Sizing from "./common/sizing.ts";
import * as Fonts from "./common/fonts.ts";

const DEFAULT_SOCIAL_IMAGE = DEFAULT_SOCIAL_CONTENT.imageUrl;

const MAX_LENGTH = {
  at4: 30,
  at3: 45,
};

const STYLES = {
  base: {
    width: "100%",
    height: "100%",
    display: "flex",
    fontFamily: "Inter",
    flexDirection: "column",
    alignItems: "center",
    backgroundImage:
      "url(https://share.unison-lang.org/static/social-image-background.png)",
    backgroundRepeat: "no-repeat",
    color: Colors.gray.darken30,
  },
};

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

async function defaultSocialImage(): Promise<React.Element> {
  return <img src={DEFAULT_SOCIAL_IMAGE} width="1200" height="630" />;
}

function truncate(maxLength: number, str: string): string {
  if (str.length > maxLength) {
    return str.slice(0, maxLength - 3) + "...";
  } else {
    return str;
  }
}

async function userSocialImage(handle: string): Promise<React.Element> {
  console.log("[UserSocialImage]", "Fetching user", handle);

  const user = await ShareAPI.getUser(handle);

  if (!user) {
    console.log("[UserSocialImage]", "User not found", handle);
    return await defaultSocialImage();
  }

  let title = user.name || user.handle;
  let titleFontSize = Sizing.toPx(4);

  if (title.length > MAX_LENGTH.at3) {
    title = truncate(MAX_LENGTH.at3, title);
  } else if (title.length > MAX_LENGTH.at4) {
    titleFontSize = Sizing.toPx(3);
  }

  let subTitle = `share.unison-lang.org/${handle}`;

  if (handle.length > MAX_LENGTH.at3) {
    subTitle = truncate(MAX_LENGTH.at3, handle);
  } else if (subTitle.length > MAX_LENGTH.at3) {
    subTitle = handle;
  }

  console.log("[UserSocialImage]", "Share API Response:", JSON.stringify(user));

  return (
    <div style={STYLES.base}>
      <div
        style={{
          display: "flex",
          flexDirection: "column",
          alignItems: "center",
          marginTop: Sizing.toPx(6),
          gap: Sizing.toPx(3),
        }}
      >
        <img
          width={Sizing.toPx(12.25)}
          height={Sizing.toPx(12.25)}
          src={
            user.avatarUrl ||
            "https://share.unison-lang.org/static/no-avatar.png"
          }
          style={{
            boxModel: "border-box",
            borderRadius: Sizing.toPx(8),
            boxShadow: `inset 0 0 0 ${Sizing.toPx(
              0.25
            )}px rgba(255, 255, 255, 0.25), 0 0 0 ${Sizing.toPx(0.25)}px ${
              Colors.gray.darken30
            }`,
          }}
        />
        <div
          style={{
            display: "flex",
            flexDirection: "column",
            alignItems: "center",
            justifyContent: "center",
            gap: Sizing.toPx(1),
          }}
        >
          <h1
            style={{
              color: Colors.gray.darken30,
              fontSize: titleFontSize,
              fontWeight: Fonts.Weights.bold,
              margin: 0,
              textShadow: `0 0 ${Sizing.toPx(0.5)}px ${Colors.gray.darken10}`,
            }}
          >
            {title}
          </h1>
          <p
            style={{
              color: Colors.gray.lighten20,
              fontSize: Sizing.toPx(3),
              fontWeight: Fonts.Weights.semiBold,
              margin: 0,
              textShadow: `0 0 ${Sizing.toPx(0.5)}px ${Colors.gray.darken10}`,
            }}
          >
            {subTitle}
          </p>
        </div>
      </div>
    </div>
  );
}

async function projectSocialImage(
  handle: string,
  projectSlug: string
): Promise<React.Element> {
  const project = await ShareAPI.getProject(handle, projectSlug);

  if (!project) return await defaultSocialImage();

  return (
    <div style={STYLES.base}>
      <h1>
        {handle}/{projectSlug}
      </h1>
      <p>{project.summary}</p>
    </div>
  );
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
