import React from "https://esm.sh/react@18.2.0";
import * as Sizing from "../common/sizing.ts";
import Colors from "../common/colors.ts";

function SocialImageWithSheet(props): React.Element {
  return <div style={STYLES.base}>{props.children}</div>;
}

const STYLES = {
  base: {
    width: "100%",
    height: "100%",
    display: "flex",
    fontFamily: "Inter",
    flexDirection: "column",
    backgroundImage:
      "url(https://share.unison-lang.org/static/social-image-background.png)",
    backgroundRepeat: "no-repeat",
    color: Colors.gray.darken30,
    paddingTop: Sizing.toPx(12.25),
  },
};

export default SocialImageWithSheet;
