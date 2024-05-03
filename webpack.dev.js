const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const webpack = require("webpack");
const postcssPresetEnv = require("postcss-preset-env");
const FaviconsWebpackPlugin = require("favicons-webpack-plugin");

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
      },
      "/website": {
        target: WEBSITE_URL,
        pathRewrite: { "^/website": "" },
        logLevel: "debug",
      },
    },
  },
};
