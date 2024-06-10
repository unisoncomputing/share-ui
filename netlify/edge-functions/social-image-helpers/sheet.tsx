import React from "https://esm.sh/react@18.2.0";
import * as Sizing from "../common/sizing.ts";
import * as Fonts from "../common/fonts.ts";
import Icon from "./icon.tsx";
import Colors from "../common/colors.ts";
import { intersperse } from "../common/utils.ts";

type SheetProps = {
  topRowLeft?: React.ReactNode;
  topRowRight?: React.ReactNode;
  title: string;
  bottomRowLeft?: React.ReactNode;
  bottomRowRight?: React.ReactNode;
  footer?: React.ReactNode;
};

function renderRow(row: React.ReactNode): React.ReactNode {
  const rowSep = <span style={STYLES.rowSeparator}>|</span>;

  const row_ = React.Children.toArray(row);

  return intersperse(
    row_.filter((e) => !!e),
    rowSep
  );
}

function Sheet(props: SheetProps) {
  return (
    <div style={STYLES.base}>
      <div style={STYLES.header}>
        <div style={STYLES.topRow}>
          <div style={STYLES.topRowInner}>
            {renderRow(props.topRowLeft || [])}
          </div>
          <div style={STYLES.topRowInner}>
            {renderRow(props.topRowRight || [])}
          </div>
        </div>
        <h1 style={STYLES.title}>{props.title}</h1>
        <div style={STYLES.bottomRow}>
          <div style={STYLES.bottomRowInner}>
            {renderRow(props.bottomRowLeft || [])}
          </div>
          <div style={STYLES.bottomRowInner}>
            {renderRow(props.bottomRowRight || [])}
          </div>
        </div>
      </div>
      <div style={STYLES.footer}>
        {props.footer || " "}
        <span style={STYLES.unisonShareLogo}>
          <Icon
            color={Colors.gray.lighten20}
            icon="unisonMark"
            size={Sizing.toPx(1.5)}
          />
          <span style={STYLES.unisonShare}>
            Unison<span style={STYLES.share}>Share</span>
          </span>
        </span>
      </div>
    </div>
  );
}

const STYLES = {
  base: {
    display: "flex",
    flexDirection: "column",
    justifyContent: "space-between",
    flex: 1,
    padding: Sizing.toPx(3),
    fontWeight: Fonts.Weights.semiBold,
    background: Colors.gray.lighten100,
  },
  header: {
    display: "flex",
    flexDirection: "column",
    gap: Sizing.toPx(1.25),
  },
  rowSeparator: {
    color: Colors.gray.lighten40,
    fontSize: Sizing.toPx(2),
    fontWeight: Fonts.Weights.regular,
  },
  topRow: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    color: Colors.gray.lighten20,
    fontSize: Sizing.toPx(2),
    lineHeight: 1,
  },
  topRowInner: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    gap: Sizing.toPx(1),
    lineHeight: 1,
  },
  title: {
    color: Colors.gray.darken30,
    lineHeight: 1.15,
    fontSize: Sizing.toPx(4.5),
    margin: 0,
  },
  bottomRow: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    justifyContent: "space-between",
    color: Colors.gray.lighten20,
    fontSize: Sizing.toPx(1.5),
    lineHeight: 1,
  },
  bottomRowInner: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    gap: Sizing.toPx(1),
    lineHeight: 1,
  },
  footer: {
    display: "flex",
    flexDirection: "row",
    alignItems: "flex-end",
    justifyContent: "space-between",
    color: Colors.gray.lighten20,
    fontSize: Sizing.toPx(1.5),
  },
  unisonShareLogo: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    gap: Sizing.toPx(0.5),
  },
  unisonShare: {
    display: "flex",
    flexDirection: "row",
    alignItems: "center",
    gap: Sizing.toPx(0.25),
    color: Colors.gray.lighten20,
    fontSize: Sizing.toPx(1.5),
  },
  share: {
    color: Colors.purple3,
  },
};

export default Sheet;
