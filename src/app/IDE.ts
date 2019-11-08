import "../BaseTypes"
import "../Graphics2"
import { openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

new Editor(openWithImports("nested-rectangles"))
