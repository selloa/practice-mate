import "../css/tailwind.css";
import { Elm } from "../elm/Main.elm";

const app = Elm.Main.init({
  node: document.getElementById("root"),
});
