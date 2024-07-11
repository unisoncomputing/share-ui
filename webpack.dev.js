const path = require("path");
const CopyPlugin = require("copy-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const postcssPresetEnv = require("postcss-preset-env");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");
const branchDiffJson = require("./api-stubs/branch-diff.json");
const definitionDiffJson = require("./api-stubs/definition-diff.json");
const definitionsByNameJson = require("./api-stubs/definitions-by-name.json");

const API_URL = process.env.API_URL || "http://127.0.0.1:5424";
const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";
const WEBSITE_URL = process.env.WEBSITE_URL || "https://www.unison-lang.org";

module.exports = {
  entry: "./src/unisonShare.js",

  resolve: {
    alias: {
      assets: path.resolve(__dirname, "src/assets/"),
      "ui-core": path.resolve(__dirname, UI_CORE_SRC + "/"),
    },
  },

  module: {
    rules: [
      {
        test: /\.css$/i,
        use: [
          "style-loader",
          {
            loader: "css-loader",
            options: { importLoaders: 1 },
          },
          {
            loader: "postcss-loader",
            options: {
              postcssOptions: {
                plugins: [
                  postcssPresetEnv({
                    features: {
                      "is-pseudo-class": false,
                      "custom-media-queries": {
                        importFrom: `${UI_CORE_SRC}/css/ui/viewport.css`,
                      },
                    },
                  }),
                ],
              },
            },
          },
        ],
      },
      {
        test: /\.md$/i,
        type: "asset/source",
      },
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
      {
        test: /\.(woff(2)?|ttf|eot)$/i,
        type: "asset/resource",
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "elm-asset-webpack-loader",
          },
          {
            loader: "elm-webpack-loader",
            options: {
              debug: false,
              cwd: __dirname,
            },
          },
        ],
      },
    ],
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: "./src/unisonShare.ejs",
      inject: "body",
      publicPath: "/",
      base: "/",
      filename: path.resolve(__dirname, "dist/dev/index.html"),
    }),

    new FaviconsWebpackPlugin({
      logo: "./src/assets/dev-favicon.svg",
      inject: true,
      favicons: {
        appName: "Unison Share",
        appDescription: "Explore, read docs about, and share Unison libraries",
        developerName: "Unison",
        developerURL: "https://unison-lang.org",
        background: "#C6A8EC",
        theme_color: "#C6A8EC",
      },
    }),

    new CopyPlugin({
      patterns: [
        {
          from: "src/assets/unison-share-social.png",
          to: "unison-share-social.png",
        },
        {
          from: "src/assets/unison-logo-circle.png",
          to: "unison-logo-circle.png",
        },
        {
          from: "src/assets/unison-logo-square.png",
          to: "unison-logo-square.png",
        },
        {
          from: "src/assets/unison-cloud-splash.svg",
          to: "unison-cloud-splash.svg",
        },
        // These are for social images in netlify/edge-functions
        {
          from: "src/assets/edge-functions/social-image-background.png",
          to: "social-image-background.png",
        },
        {
          from: "src/assets/edge-functions/no-avatar.png",
          to: "no-avatar.png",
        },
        {
          from: "src/assets/edge-functions/Inter-Regular.ttf",
          to: "Inter-Regular.ttf",
        },
        {
          from: "src/assets/edge-functions/Inter-Bold.ttf",
          to: "Inter-Bold.ttf",
        },
        {
          from: "src/assets/edge-functions/Inter-Black.ttf",
          to: "Inter-Black.ttf",
        },
        {
          from: "src/assets/edge-functions/FiraCode-Regular.ttf",
          to: "FiraCode-Regular.ttf",
        },
        {
          from: "src/assets/edge-functions/FiraCode-Bold.ttf",
          to: "FiraCode-Bold.ttf",
        },
        {
          from: "src/robots.txt",
          to: path.resolve(__dirname, "dist/unisonShare/robots.txt"),
        },
        {
          from: "src/sitemap.txt",
          to: path.resolve(__dirname, "dist/unisonShare/sitemap.txt"),
        },
        {
          from: "src/404.html",
          to: path.resolve(__dirname, "dist/unisonShare/404.html"),
        },
        {
          from: "src/500.html",
          to: path.resolve(__dirname, "dist/unisonShare/500.html"),
        },
        {
          from: "src/maintenance.html",
          to: path.resolve(__dirname, "dist/unisonShare/maintenance.html"),
        },
      ],
    }),

    new webpack.DefinePlugin({
      API_URL: JSON.stringify("api"),
      WEBSITE_URL: JSON.stringify("website"),
      APP_ENV: JSON.stringify("development"),
    }),
  ],

  output: {
    filename: "[name].[contenthash].js",
    path: path.resolve(__dirname, "dist/dev"),
    clean: true,
  },

  devServer: {
    historyApiFallback: {
      disableDotRule: true,
    },
    proxy: {
      "/api": {
        target: API_URL,
        pathRewrite: { "^/api": "" },
        logLevel: "debug",
        bypass: (req, res, _proxyOptions) => {
          if (req.url.endsWith("/diff")) {
            res.send(branchDiffJson);
          }
          if (req.url.includes("/diff/terms")) {
            res.send(definitionDiffJson);
          }
          if (req.url.includes("/definitions/by-name")) {
            res.send(definitionsByNameJson);
          }
        },
      },
      "/website": {
        target: WEBSITE_URL,
        pathRewrite: { "^/website": "" },
        logLevel: "debug",
      },
    },
  },
};
