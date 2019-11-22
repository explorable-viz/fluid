import "../BaseTypes"
import "../Graphics2"
import { Env } from "../Env"
import { Expr } from "../Expr"
import { openDatasetAs, openWithImports2 } from "../Module"
import "../app/GraphicsRenderer2"
import { Editor } from "./Editor"

const ρ: Env = openDatasetAs("renewables", "data")
const [ρʹ, e]: [Env, Expr] = openWithImports2("graphics/grouped-bar-chart")
new Editor(false, e, ρ.concat(ρʹ))
