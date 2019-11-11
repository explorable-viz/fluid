import "../BaseTypes"
import "../Graphics2"
import { openDatasetAs, openWithImports } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

new Editor(openWithImports("graphics/grouped-bar-chart"), openDatasetAs("renewables", "data"))
