import postcssGlobalData from "@csstools/postcss-global-data";
import postcssPresetEnv from "postcss-preset-env";

const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";

export default {
  plugins: [
    postcssGlobalData({
      files: [`${UI_CORE_SRC}/css/ui/viewport.css`],
    }),
    postcssPresetEnv({
      features: {
        "is-pseudo-class": false,
        "nesting-rules": true,
        "has-pseudo-class": true,
      },
    }),
  ],
};
