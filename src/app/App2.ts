import { __nonNull, as } from "../util/Core"
import { World } from "../util/Versioned"
import { List} from "../BaseTypes"
import { Eval } from "../Eval"
import { Value } from "../ExplVal"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { ρ, initialise, load, parse } from "../../test/util/Core"
import { Cursor } from "../../test/util/Cursor"
import { Data } from "./DataRenderer"
import { GraphicsPane3D2 } from "./GraphicsPane3D2"
import { GraphicsRenderer } from "./GraphicsRenderer"
import { reflect} from "./Reflect"

class App2 {
   canvas: HTMLCanvasElement
   graphicsPane3D: GraphicsPane3D2
   
   constructor () {
      this.canvas = document.createElement("canvas")
      this.graphicsPane3D = new GraphicsPane3D2(window.innerWidth, window.innerHeight / 2)
   }

   initialise () {
      initialise()
      document.body.appendChild(this.graphicsPane3D.renderer.domElement)
      this.graphicsPane3D.setCanvas(this.canvas)
      this.canvas.width = this.canvas.height = 256
      const [, g]: [Data, GraphicsElement] = this.loadExample()
      this.render(g)
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

   render (g: GraphicsElement) {
      this.renderGraphic(g)
      this.graphicsPane3D.texture.needsUpdate = true
      this.graphicsPane3D.mesh.rotation.y += 1
      this.graphicsPane3D.renderer.render(this.graphicsPane3D.scene, this.graphicsPane3D.camera)
   }

   renderGraphic (g: GraphicsElement): void {
      new GraphicsRenderer(this.canvas).render(g)
   }

   renderCanvas () {
      const ctx: CanvasRenderingContext2D = __nonNull(this.canvas.getContext('2d'))
      ctx.font = '20pt Arial'
      ctx.fillStyle = 'red'
      ctx.fillRect(0, 0, this.canvas.width, this.canvas.height)
      ctx.fillStyle = 'white'
      ctx.fillRect(10, 10, this.canvas.width - 20, this.canvas.height - 20)
      ctx.fillStyle = 'black'
      ctx.textAlign = "center"
      ctx.textBaseline = "middle"
      ctx.fillText(new Date().getTime().toString(), this.canvas.width / 2, this.canvas.height / 2)
   }
}

new App2().initialise()
