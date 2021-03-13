import { Elm } from "../elm/Main.elm";

const STORAGE_ID = "configuration";

const storedData = localStorage.getItem(STORAGE_ID);

const url = new URL(location);
const sp = new URLSearchParams(url.search);

const flags = isInvalidConfig(urlParamsToConfiguration(sp))
  ? storedData
    ? JSON.parse(storedData)
    : null
  : urlParamsToConfiguration(sp);

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: flags,
});

app.ports &&
  app.ports.printToConsole &&
  app.ports.printToConsole.subscribe(function (message) {
    console.log(message);
  });

app.ports.setStorage.subscribe(function (configuration) {
  localStorage.setItem(STORAGE_ID, JSON.stringify(configuration));
});

app.ports.setLink.subscribe(function (configuration) {
  window.location = `${window.origin}?${configurationToUrlParams(
    configuration
  )}`;
});

function urlParamsToConfiguration(sp) {
  // todo preset removed so always custom
  const CONFIGURATION_KEYS = [
    "topics",
    "roots",
    "scales",
    "intervals",
    "challenges",
    "bowings",
    "chords",
    "preset",
  ];

  return [
    {
      ...CONFIGURATION_KEYS.map((key) => {
        const elements = sp.has(key) ? sp.get(key).split(",") : [""];

        return { [key]: elements };
      }).reduce((config, category) => ({ ...config, ...category }), {}),
      preset: sp.get("preset") || "",
    },
  ].map((c) => ({
    ...c,
    bowings: c.bowings.map((b) => ({
      kind: "Slurred",
      times: parseInt(b, 10),
    })),
  }))[0];
}

function configurationToUrlParams(configuration) {
  return [configuration]
    .map((c) => ({
      ...c,
      preset: [c.preset],
      bowings: [c.bowings.map((b) => b.times)],
    }))
    .map((c) =>
      Object.keys(c).map((key) => {
        console.log(c[key]);
        return `${key}=${arrayToCSV(c[key])}`;
      })
    )[0]
    .reduce((result, topicString) => (result += `&${topicString}`), "")
    .slice(1);
}

function arrayToCSV(array) {
  return array
    .reduce((result, el) => {
      return (result += `,${el}`);
    }, "")
    .slice(1);
}

function isInvalidConfig(config) {
  return (
    config.topics.length === 0 &&
    config.challenges.length === 0 &&
    config.intervals.length === 0 &&
    config.roots.length === 0 &&
    config.chords.length === 0 &&
    config.bowings.length === 0 &&
    config.scales.length === 0
  );
}
