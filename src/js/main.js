import { Elm } from "../elm/Main.elm";

const STORAGE_ID = "configuration";

const storedData = localStorage.getItem(STORAGE_ID);
const flags = storedData ? JSON.parse(storedData) : null;

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
    .reduce((result, topicString) => (result += `&${topicString}`), "");
}

function arrayToCSV(array) {
  return array
    .reduce((result, el) => {
      return (result += `,${el}`);
    }, "")
    .slice(1);
}
