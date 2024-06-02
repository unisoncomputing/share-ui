import React from "https://esm.sh/react@18.2.0";
import ShareAPI from "../common/share-api.ts";
import Colors from "../common/colors.ts";
import * as Sizing from "../common/sizing.ts";
import * as Fonts from "../common/fonts.ts";
import {
  SocialImageStyles,
  defaultSocialImage,
  MaxLength,
} from "./social-content.tsx";
import { truncate } from "../common/utils.ts";

async function userSocialImage(handle: string): Promise<React.Element> {
  console.log("[UserSocialImage]", "Fetching user", handle);

  const user = await ShareAPI.getUser(handle);

  if (!user) {
    console.log("[UserSocialImage]", "User not found", handle);
    return await defaultSocialImage();
  }

  let title = user.name || user.handle;
  let titleFontSize = Sizing.toPx(4);

  if (title.length > MaxLength.at3) {
    title = truncate(MaxLength.at3, title);
  } else if (title.length > MaxLength.at4) {
    titleFontSize = Sizing.toPx(3);
  }

  let subTitle = `share.unison-lang.org/${handle}`;

  if (handle.length > MaxLength.at3) {
    subTitle = truncate(MaxLength.at3, handle);
  } else if (subTitle.length > MaxLength.at3) {
    subTitle = handle;
  }

  return (
    <div style={SocialImageStyles.base}>
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
            }}
          >
            {subTitle}
          </p>
        </div>
      </div>
    </div>
  );
}

export default userSocialImage;
