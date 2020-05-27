import "../css/tailwind.css";
import { Elm } from "../elm/Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});

app.ports.printToConsole.subscribe(function (message) {
  console.log(message);
});
