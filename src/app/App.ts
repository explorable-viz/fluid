import { as } from "../util/Core"
import { World } from "../util/Versioned"
import { List } from "../BaseTypes"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { GraphicsElement } from "../Graphics"
import { Value } from "../ExplVal"
// TODO: move test-dependent stuff out of app
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { Data, DataRenderer } from "./DataRenderer"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer } from "./GraphicsRenderer"
import { reflect } from "./Reflect"

const graphicsPane3D = new GraphicsPane3D(),
      dataCanvas: HTMLCanvasElement = document.createElement("canvas"),
      graphCanvas: HTMLCanvasElement = document.createElement("canvas")
   
initialise()
initialiseScene()
populateScene()
graphicsPane3D.render()

function initialiseScene (): void {
   dataCanvas.style.verticalAlign = "top"
   dataCanvas.style.display = "inline-block"
   graphCanvas.height = 600
   graphCanvas.width = 600
   graphCanvas.style.verticalAlign = "top"
   graphCanvas.style.display = "inline-block"
   document.body.appendChild(dataCanvas)
   document.body.appendChild(graphCanvas)
   document.body.appendChild(graphicsPane3D.renderer.domElement)
}

function populateScene (): void {
   const e: Expr = parse(load("bar-chart"))
   World.newRevision()
   let here: Cursor = new Cursor(e)
   here
      .skipImports()
      .to(Expr.Let, "e")
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1)
      .constrArg("Cons", 0)
      .constrArg("Pair", 1).notNeed() // 2015 > China > Bio > [here]
   here = new Cursor(e)
      .skipImports()
      .to(Expr.Let, "e")
   const data: Value.Constr = as(Eval.eval_(ρ, as(here.o, Expr.Constr)).v, Value.Constr), // eval just to get a handle on it
         v: Value = Eval.eval_(ρ, e).v,
         elem: GraphicsElement = as(reflect(v), GraphicsElement),
         graphRenderer: GraphicsRenderer = new GraphicsRenderer(graphCanvas)
   graphRenderer.render(elem)
   // TODO: when backward slicing, will have to "re-get" the state of data to pick up the slicing information; not nice.
   const dataRenderer = new DataRenderer(dataCanvas),
         dataʹ: Data = as(reflect(data), List)
   dataRenderer.render(dataʹ) // draw once to compute size
   dataCanvas.height = (dataRenderer.lines) * dataRenderer.lineHeight
   dataRenderer.render(dataʹ) // draw again
   graphicsPane3D.setPane(graphCanvas)
}
