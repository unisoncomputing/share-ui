import cssHasPseudo from "css-has-pseudo/browser";

cssHasPseudo(document);

import "ui-core/css/ui.css";
import "ui-core/css/themes/unison-light.css";
import "ui-core/css/code.css";
// Include web components
import "ui-core/UI/CopyOnClick";
import "ui-core/UI/ModalOverlay";
import "ui-core/UI/CopyrightYear";
import "ui-core/Lib/OnClickOutside";
import "ui-core/Lib/EmbedKatex";
import "ui-core/Lib/MermaidDiagram";
import "ui-core/Lib/EmbedSvg";
import detectOs from "ui-core/Lib/detectOs";
import preventDefaultGlobalKeyboardEvents from "ui-core/Lib/preventDefaultGlobalKeyboardEvents";
import * as Sentry from "@sentry/browser";
import { BrowserTracing } from "@sentry/tracing";
import * as Metrics from "./metrics";

import "./UnisonShare/SupportChatWidget";
import { getCookie } from "./util";

import "./css/unison-share.css";
import { Elm } from "./UnisonShare.elm";

console.log(`
 _____     _
|  |  |___|_|___ ___ ___
|  |  |   | |_ -| . |   |
|_____|_|_|_|___|___|_|_|


`);

// ----------------------------------------------------------------------------

preventDefaultGlobalKeyboardEvents();

Metrics.init();

if (APP_ENV === "production") {
  Sentry.init({
    dsn: "https://8eb2ee6bb78d4131bdbb1b6a70f6b0c0@o4503934538547200.ingest.sentry.io/4504458036903936",
    integrations: [new BrowserTracing()],
    sampleRate: 0.25,
    environment: APP_ENV,
  });
}

// ----------------------------------------------------------------------------

const basePath = new URL(document.baseURI).pathname;

const Storage = {
  backupStorage: new Map(),

  mode() {
    try {
      localStorage.length;
      return "localStorage";
    } catch (e) {
      try {
        sessionStorage.length;
        return "sessionStorage";
      } catch (e) {
        return "backupStorage";
      }
    }
  },

  set(key, val) {
    const mode = Storage.mode();
    if (mode === "backupStorage") {
      Storage.backupStorage.set(key, val);
    } else {
      window[mode].setItem(key, val);
    }
  },

  get(key) {
    const mode = Storage.mode();
    if (mode === "backupStorage") {
      return Storage.backupStorage.get(key) || null;
    } else {
      return window[mode].getItem(key);
    }
  },
};

const whatsNewReadPostIds =
  JSON.parse(Storage.get("whatsNewReadPostIds")) || [];

const flags = {
  operatingSystem: detectOs(window.navigator),
  basePath,
  apiUrl: API_URL,
  websiteUrl: WEBSITE_URL,
  xsrfToken: getCookie("XSRF-TOKEN"),
  appEnv: APP_ENV,
  whatsNewReadPostIds,
};

// The main entry point for the `UnisonShare` target of the Codebase UI.
const app = Elm.UnisonShare.init({ flags });

// Ports can be dead code eliminated, so we have to check if they exist
if (app.ports) {
  app.ports.updateWhatsNewReadPostIds?.subscribe((postIds) => {
    Storage.set("whatsNewReadPostIds", JSON.stringify(postIds));
  });
}
