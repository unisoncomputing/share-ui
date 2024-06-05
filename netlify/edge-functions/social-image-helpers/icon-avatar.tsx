import React from "https://esm.sh/react@18.2.0";
import Colors from "../common/colors.ts";
import * as Sizing from "../common/sizing.ts";
import * as Fonts from "../common/fonts.ts";
import { IconType, Icon } from "./icon.tsx";

type IconAvatarProps = {
  icon: IconType;
  iconCount?: number;
};

function IconAvatar(props: IconAvatarProps): React.Element {
  let count = null;

  if (props.iconCount) {
    const iconCount = props.iconCount > 99 ? "*" : props.iconCount;
    count = (
      <div
        style={{
          position: "absolute",
          top: `-${Sizing.toPx(3)}px`,
          right: `-${Sizing.toPx(3)}px`,
          width: Sizing.toPx(6),
          height: Sizing.toPx(6),
          borderRadius: Sizing.toPx(3),
          backgroundImage: `linear-gradient(to bottom, ${Colors.purple3}, ${Colors.purple2})`,
          boxModel: "border-box",
          boxShadow: `inset 0 0 0 ${Sizing.toPx(
            0.25
          )}px rgba(255, 255, 255, 0.25), 0 0 0 ${Sizing.toPx(0.25)}px ${
            Colors.gray.darken30
          }`,
          display: "flex",
          alignItems: "center",
          justifyContent: "center",
          fontSize: Sizing.toPx(2.625),
          fontWeight: Fonts.Weights.bold,
          color: Colors.gray.lighten100,
        }}
      >
        {iconCount}
      </div>
    );
  }

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
      {count}
    </div>
  );
}

export default IconAvatar;
