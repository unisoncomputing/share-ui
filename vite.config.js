import { defineConfig } from "vite";
import path from "path";
import { fileURLToPath } from "url";
import { plugin as elm } from "vite-plugin-elm";
import { viteStaticCopy } from "vite-plugin-static-copy";
import fs from "fs";

// API Stubs
import docExamplesReadme from "./api-stubs/doc-examples-readme.json" with { type: "json" };
import docExamplesReadmeDef from "./api-stubs/doc-examples-readme-definition.json" with { type: "json" };
import blogEngineLib from "./api-stubs/blog-engine-lib.json" with { type: "json" };
import browseBlog from "./api-stubs/browse-blog.json" with { type: "json" };
import notifications from "./api-stubs/notifications.json" with { type: "json" };
import branchDiff from "./api-stubs/branch-diff-weird-equals.json" with { type: "json" };

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const UI_CORE_SRC = "elm-stuff/gitdeps/github.com/unisonweb/ui-core/src";

export default defineConfig(({ mode, command }) => {
  const isDev = mode === "development";
  const withApiStubs = process.env.WITH_API_STUBS === "true";

  return {
    base: "/",

    resolve: {
      alias: {
        assets: path.resolve(__dirname, "src/assets/"),
        "ui-core": path.resolve(__dirname, UI_CORE_SRC + "/"),
      },
    },

    define: {
      API_URL: JSON.stringify(
        isDev ? "api" : process.env.API_URL || "https://api.unison-lang.org",
      ),
      WEBSITE_URL: JSON.stringify(
        isDev
          ? "website"
          : process.env.WEBSITE_URL || "https://www.unison-lang.org",
      ),
      APP_ENV: JSON.stringify(isDev ? "development" : "production"),
    },

    plugins: [
      elm({
        debug: isDev,
        optimize: !isDev,
      }),

      viteStaticCopy({
        targets: [
          // Favicon (goes to root, not static subdirectory)
          {
            src: isDev
              ? "src/assets/dev-favicon.svg"
              : "src/assets/favicon.svg",
            dest: "",
            rename: "favicon.svg",
          },
          // Social images
          {
            src: "src/assets/unison-share-social.png",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/unison-logo-circle.png",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/unison-logo-square.png",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/unison-cloud-splash.svg",
            dest: isDev ? "" : "static",
          },
          // Edge function assets
          {
            src: "src/assets/edge-functions/social-image-background.png",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/no-avatar.png",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/Inter-Regular.ttf",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/Inter-Bold.ttf",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/Inter-Black.ttf",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/FiraCode-Regular.ttf",
            dest: isDev ? "" : "static",
          },
          {
            src: "src/assets/edge-functions/FiraCode-Bold.ttf",
            dest: isDev ? "" : "static",
          },
          // Static files (go to root, not static subdirectory)
          {
            src: "src/robots.txt",
            dest: "",
          },
          {
            src: "src/sitemap.txt",
            dest: "",
          },
          {
            src: "src/404.html",
            dest: "",
          },
          {
            src: "src/500.html",
            dest: "",
          },
          {
            src: "src/maintenance.html",
            dest: "",
          },
        ],
      }),

      // Markdown loader plugin
      {
        name: "markdown-loader",
        transform(code, id) {
          if (id.endsWith(".md")) {
            return {
              code: `export default ${JSON.stringify(code)}`,
              map: null,
            };
          }
        },
      },

      // Inject correct base path into HTML
      {
        name: "html-base-injector",
        transformIndexHtml(html) {
          const basePath = "/";
          return html.replace(
            /<base href="[^"]*" \/>/,
            `<base href="${basePath}" />`,
          );
        },
      },
    ],

    build: {
      outDir: isDev ? "dist/dev" : "dist/unisonShare",
      emptyOutDir: true,
      rollupOptions: {
        output: {
          entryFileNames: isDev
            ? "[name].[hash].js"
            : "static/[name].[hash].js",
          chunkFileNames: isDev
            ? "[name].[hash].js"
            : "static/[name].[hash].js",
          assetFileNames: isDev
            ? "[name].[hash].[ext]"
            : "static/[name].[hash].[ext]",
        },
      },
    },

    server: {
      port: 1234,
      proxy: {
        "/api": {
          target: process.env.API_URL || "http://127.0.0.1:5424",
          changeOrigin: true,
          rewrite: (path) => path.replace(/^\/api/, ""),
        },
        "/website": {
          target: process.env.WEBSITE_URL || "https://www.unison-lang.org",
          changeOrigin: true,
          rewrite: (path) => path.replace(/^\/website/, ""),
        },
      },
    },

    preview: {
      port: 1234,
    },

    configureServer(server) {
      if (withApiStubs) {
        server.middlewares.use((req, res, next) => {
          if (req.url.endsWith("/by-name/README")) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(docExamplesReadmeDef));
            return;
          }

          if (req.url.endsWith("/readme")) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(docExamplesReadme));
            return;
          }

          if (req.url.endsWith("/browse?namespace=lib")) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(blogEngineLib));
            return;
          }

          if (req.url.endsWith("/browse")) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(browseBlog));
            return;
          }

          if (req.url.endsWith("/diff")) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(branchDiff));
            return;
          }

          if (
            req.url.endsWith("/notifications/hub?status=read%2Cunread&limit=12")
          ) {
            res.setHeader("Content-Type", "application/json");
            res.end(JSON.stringify(notifications));
            return;
          }

          next();
        });
      }
    },
  };
});
