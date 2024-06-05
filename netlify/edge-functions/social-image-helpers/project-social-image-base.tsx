import React from "https://esm.sh/react@18.2.0";
import * as Fonts from "../common/fonts.ts";
import Colors from "../common/colors.ts";
import { SocialImageStyles } from "./social-content.tsx";
import IconAvatar from "./icon-avatar.tsx";
import { IconType } from "./icon.tsx";
import * as Sizing from "../common/sizing.ts";

type ProjectSocialImageBaseProps = {
  title: string;
  titleFontSize?: number;
  summary: string;
  summaryFontSize?: number;
  icon: IconType;
  iconCount?: number;
};

function ProjectSocialImageBase(
  props: ProjectSocialImageBaseProps
): React.Element {
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
        <IconAvatar icon={props.icon} iconCount={props.iconCount} />
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
              fontSize: props.titleFontSize || Sizing.toPx(4),
              fontWeight: Fonts.Weights.bold,
              margin: 0,
            }}
          >
            {props.title}
          </h1>
          <p
            style={{
              color: Colors.gray.lighten20,
              fontSize: props.summaryFontSize || Sizing.toPx(3),
              fontWeight: Fonts.Weights.semiBold,
              margin: 0,
              marginLeft: Sizing.toPx(2),
              marginRight: Sizing.toPx(2),
              lineHeight: 1.4,
              textAlign: "center",
              maxWidth: "100%",
            }}
          >
            {props.summary}
          </p>
        </div>
      </div>
    </div>
  );
}

export default ProjectSocialImageBase;
