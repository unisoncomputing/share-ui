import Plausible from "plausible-tracker";

let plausible;

function init() {
  if (APP_ENV === "production") {
    plausible = Plausible({
      domain: "share.unison-lang.org",
    });

    plausible.enableAutoPageviews();
  } else {
    console.log(`[${APP_ENV}] Metrics.init`);
  }
}

function track(evt) {
  if (APP_ENV === "production") {
    if (evt.data) {
      plausible(evt.event, { props: evt.data });
    } else {
      plausible(evt.event);
    }
  } else if (evt.data) {
    console.log(`[${APP_ENV}] Metrics.track: ${evt.event} ->`, evt.data);
  } else {
    console.log(`[${APP_ENV}] Metrics.track: ${evt.event}`);
  }
}

export { init, track };
