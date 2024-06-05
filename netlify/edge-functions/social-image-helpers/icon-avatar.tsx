import React from "https://esm.sh/react@18.2.0";
import Colors from "../common/colors.ts";
import * as Sizing from "../common/sizing.ts";
import { IconType, Icon } from "./icon.tsx";

function IconAvatar(props: { icon: IconType }): React.Element {
  return (
    <div
      style={{
        width: Sizing.toPx(12.25),
        height: Sizing.toPx(12.25),
        borderRadius: Sizing.toPx(2),
        backgroundImage: `linear-gradient(to bottom, ${Colors.blue4}, ${Colors.purple5})`,
        boxModel: "border-box",
        boxShadow: `inset 0 0 0 ${Sizing.toPx(
          0.25
        )}px rgba(255, 255, 255, 0.25), 0 0 0 ${Sizing.toPx(0.25)}px ${
          Colors.gray.darken30
        }`,
        display: "flex",
        alignItems: "center",
        justifyContent: "center",
      }}
    >
      <Icon
        icon={props.icon}
        color={Colors.gray.darken30}
        size={Sizing.toPx(8)}
      />
    </div>
  );
}

export default IconAvatar;
