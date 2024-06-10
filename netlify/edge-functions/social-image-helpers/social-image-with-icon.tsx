import React from "https://esm.sh/react@18.2.0";
import * as Fonts from "../common/fonts.ts";
import Colors from "../common/colors.ts";
import IconAvatar from "./icon-avatar.tsx";
import { IconType } from "./icon.tsx";
import * as Sizing from "../common/sizing.ts";

type SocialImageWithIconProps = {
  title: string;
  titleFontSize?: number;
  summary: string;
  summaryFontSize?: number;
  icon: IconType;
  iconCount?: number;
};

function SocialImageWithIcon(props: SocialImageWithIconProps): React.Element {
  return (
    <div style={STYLES.base}>
      <div style={STYLES.innerBase}>
        <IconAvatar icon={props.icon} iconCount={props.iconCount} />
        <div style={STYLES.text}>
          <h1 style={STYLES.title(props.titleFontSize)}>{props.title}</h1>
          <div style={STYLES.summary(props.summaryFontSize)}>
            {props.summary}
          </div>
        </div>
      </div>
    </div>
  );
}

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
  innerBase: {
    display: "flex",
    flexDirection: "column",
    alignItems: "center",
    justifyContent: "center",
    marginTop: Sizing.toPx(6),
    gap: Sizing.toPx(3),
    fontSize: Sizing.toPx(2.5),
  },
  text: {
    display: "flex",
    flexDirection: "column",
    alignItems: "center",
    justifyContent: "center",
    gap: Sizing.toPx(1),
    textAlign: "center",
  },
  title(titleFontSize?: number) {
    return {
      color: Colors.gray.darken30,
      fontSize: titleFontSize || Sizing.toPx(4),
      fontWeight: Fonts.Weights.bold,
      margin: 0,
    };
  },
  summary(summaryFontSize?: number) {
    return {
      color: Colors.gray.lighten20,
      fontSize: summaryFontSize || Sizing.toPx(3),
      fontWeight: Fonts.Weights.semiBold,
      marginLeft: Sizing.toPx(2),
      marginRight: Sizing.toPx(2),
      lineHeight: 1.4,
      maxWidth: "100%",
      textAlign: "center",
    };
  },
};

export default SocialImageWithIcon;
