import React from "https://esm.sh/react@18.2.0";
import * as Sizing from "../common/sizing.ts";
import Colors from "../common/colors.ts";
import * as Fonts from "../common/fonts.ts";

type TagColor = "green" | "pink" | "blue" | "orange" | "purple" | "gray";

type TagProps = {
  color: TagColor;
  text: string;
};

function Tag(props: TagProps): React.Element {
  return <span style={STYLES.base(props.color)}>{props.text}</span>;
}

const STYLES = {
  base(color: TagColor) {
    return {
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      fontSize: Sizing.toPx(1.5),
      padding: `${Sizing.toPx(0.5)}px ${Sizing.toPx(1.5)}px`,
      fontWeight: Fonts.Weights.semiBold,
      borderRadius: Sizing.toPx(3),
      lineHeight: 1,
      ...STYLES[color],
    };
  },
  green: {
    color: Colors.green1,
    backgroundColor: Colors.green5,
    border: `2px solid ${Colors.green2}`,
  },
  blue: {
    color: Colors.blue1,
    backgroundColor: Colors.blue5,
    border: `2px solid ${Colors.blue2}`,
  },
  pink: {
    color: Colors.pink1,
    backgroundColor: Colors.pink5,
    border: `2px solid ${Colors.pink2}`,
  },
  orange: {
    color: Colors.orange0,
    backgroundColor: Colors.orange5,
    border: `2px solid ${Colors.orange1}`,
  },
  purple: {
    color: Colors.purple1,
    backgroundColor: Colors.purple5,
    border: `2px solid ${Colors.purple2}`,
  },
  gray: {
    color: Colors.gray.darken30,
    backgroundColor: Colors.gray.lighten45,
    border: `2px solid ${Colors.gray.lighten20}`,
  },
};

export default Tag;
export { Tag, TagColor };
