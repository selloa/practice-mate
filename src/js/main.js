import { Elm } from "../elm/Main.elm";

const STORAGE_ID = "configuration";

const storedData = localStorage.getItem(STORAGE_ID);

console.log(urlParamsToConfiguration(window.location.href));

const flags = {
  storedConfiguration: storedData ? JSON.parse(storedData) : null,
  urlConfiguration: urlParamsToConfiguration(window.location.href),
};

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
  console.log(configuration);
  console.log(configurationToUrlParams(configuration));
  window.location = `${window.origin}?${configurationToUrlParams(
    configuration
  )}`;
});

function urlParamsToConfiguration(location) {
  const url = new URL(location);
  const sp = new URLSearchParams(url.search);

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

  return {
    ...CONFIGURATION_KEYS.map((key) => {
      const elements = sp.has(key) ? sp.get(key).split(",") : [];

      return { [key]: elements };
    }).reduce((config, category) => ({ ...config, ...category }), {}),
    preset: sp.get("preset") || "",
  };
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
