import "../BaseTypes"
import "../Graphics"
import { module_graphics, openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer"
import { Editor } from "./Editor"

new Editor(openWithImports("bar-chart", module_graphics), openDatasetAs("renewables", "data"))
