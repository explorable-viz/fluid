import { as } from "../util/Core"
import { World } from "../util/Versioned"
import { List} from "../BaseTypes"
import { Eval } from "../Eval"
import { Value } from "../ExplVal"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { Cursor } from "../../test/util/Cursor"
import { Data, DataView, DataRenderer } from "./DataRenderer"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer } from "./GraphicsRenderer"
import { reflect} from "./Reflect"

class App2 {
   dataCanvas: HTMLCanvasElement
   graphicsCanvas: HTMLCanvasElement
   graphicsPane3D: GraphicsPane3D
   
   constructor () {
      this.dataCanvas = document.createElement("canvas")
      this.graphicsCanvas = document.createElement("canvas")
      this.graphicsPane3D = new GraphicsPane3D(600, 600)
   }

   initialise () {
      initialise()
      this.dataCanvas.style.verticalAlign = "top"
      this.dataCanvas.style.display = "inline-block"
      this.graphicsPane3D.renderer.domElement.style.verticalAlign = "top"
      this.graphicsPane3D.renderer.domElement.style.display = "inline-block"
      document.body.appendChild(this.dataCanvas)
      document.body.appendChild(this.graphicsCanvas)
      // document.body.appendChild(this.graphicsPane3D.renderer.domElement)
      this.graphicsPane3D.setCanvas(this.graphicsCanvas)
      this.graphicsCanvas.width = this.graphicsCanvas.height = 256
      this.render()
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

   render () {
      const [data, g]: [Data, GraphicsElement] = this.loadExample()
      this.renderData(data)
      this.renderGraphic(g)
      // this.graphicsPane3D.render()
   }

   // TODO: when backward slicing, will have to "re-get" the state of data to pick up the slicing information; not nice.
   renderData (data: Data): void {
      this.dataCanvas.height = 400
      this.dataCanvas.width = 400
      const view: DataView = new DataRenderer(this.dataCanvas, data).view
      this.dataCanvas.height = view.height + 1 // not sure why extra pixel is essential
      this.dataCanvas.width = view.width
      view.draw()
   }

   renderGraphic (g: GraphicsElement): void {
      new GraphicsRenderer(this.graphicsCanvas).render(g)
   }
}

new App2().initialise()
