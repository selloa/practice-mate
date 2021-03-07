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
