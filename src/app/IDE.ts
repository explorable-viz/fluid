import "../BaseTypes"
import "../Graphics"
import { openWithImports } from "../Module"
import "../app/GraphicsRenderer"
import { Editor } from "./Editor"

new Editor(openWithImports("lookup"))
