import { as } from "../util/Core"
import { World } from "../util/Versioned"
import { List } from "../BaseTypes"
import { Expr } from "../Expr"
import { Eval } from "../Eval"
import { GraphicsElement } from "../Graphics"
import { Value } from "../ExplVal"
import { Cursor } from "../../test/util/Cursor"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { Data, DataRenderer } from "./DataRenderer"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer } from "./GraphicsRenderer"
import { reflect } from "./Reflect"

class App {
   graphicsPane3D: GraphicsPane3D
   dataCanvas: HTMLCanvasElement
   graphCanvas: HTMLCanvasElement

   constructor () {
      this.graphicsPane3D = new GraphicsPane3D()
      this.dataCanvas = document.createElement("canvas"),
      this.graphCanvas = document.createElement("canvas")
   }

   initialise (): void {
      initialise()
      this.initialiseScene()
      const [data, g]: [Data, GraphicsElement] = this.loadExample()
      this.renderData(data)
      this.renderGraphic(g)
   }

   initialiseScene (): void {
      this.dataCanvas.style.verticalAlign = "top"
      this.dataCanvas.style.display = "inline-block"
      this.graphCanvas.height = 600
      this.graphCanvas.width = 600
      this.graphCanvas.style.verticalAlign = "top"
      this.graphCanvas.style.display = "inline-block"
      document.body.appendChild(this.dataCanvas)
      document.body.appendChild(this.graphCanvas)
      document.body.appendChild(this.graphicsPane3D.renderer.domElement)
   }   

   loadExample (): [Data, GraphicsElement] {
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
            v: Value = Eval.eval_(ρ, e).v
      return [as(reflect(data), List), as(reflect(v), GraphicsElement)]
   }

   // TODO: when backward slicing, will have to "re-get" the state of data to pick up the slicing information; not nice.
   renderData (data: Data): void {
      const dataRenderer = new DataRenderer(this.dataCanvas)
      dataRenderer.render(data) // draw once to compute size
      this.dataCanvas.height = (dataRenderer.lines) * dataRenderer.lineHeight
      dataRenderer.render(data) // draw again
       this.graphicsPane3D.setPane(this.graphCanvas)
   }

   renderGraphic (g: GraphicsElement): void {
      new GraphicsRenderer(this.graphCanvas).render(g)
      this.graphicsPane3D.render()
   }
}
   
new App().initialise()
