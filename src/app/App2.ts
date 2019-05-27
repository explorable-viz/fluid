import { ann } from "../util/Annotated"
import { __nonNull, as } from "../util/Core"
import { Cons, List } from "../BaseTypes2"
import { emptyEnv } from "../Env2"
import { Eval } from "../Eval2"
import { Expl, ExplValue } from "../ExplValue2"
import { Expr } from "../Expr2"
import { GraphicsElement } from "../Graphics2"
import { Value } from "../Value2"
import { setallα } from "../Versioned2"
import { load, parse } from "../../test/util/Core2"
import { Cursor } from "../../test/util/Cursor2"
import { Data, DataView, DataRenderer } from "./DataRenderer2"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer } from "./GraphicsRenderer2"

class App {
   e: Expr                        // entire closed program
   tv: ExplValue                  // chart computed by program
   data_e: Expr                   // expression for data (value bound by first let in user code)
   data_tv: ExplValue             // value of data
   dataView: DataView
   dataCanvas: HTMLCanvasElement
   dataCtx: CanvasRenderingContext2D
   graphicsCanvas: HTMLCanvasElement
   graphicsPane3D: GraphicsPane3D
   
   constructor () {
      this.dataCanvas = document.createElement("canvas")
      this.dataCtx = __nonNull(this.dataCanvas.getContext("2d"))
      this.graphicsCanvas = document.createElement("canvas")
      this.graphicsPane3D = new GraphicsPane3D(600, 600)
      this.dataCanvas.style.verticalAlign = "top"
      this.dataCanvas.style.display = "inline-block"
      this.graphicsPane3D.renderer.domElement.style.verticalAlign = "top"
      this.graphicsPane3D.renderer.domElement.style.display = "inline-block"
      document.body.appendChild(this.dataCanvas)
      document.body.appendChild(this.graphicsCanvas)
      // document.body.appendChild(this.graphicsPane3D.renderer.domElement)
      this.graphicsPane3D.setCanvas(this.graphicsCanvas)
      this.graphicsCanvas.width = this.graphicsCanvas.height = 400
      this.loadExample()
   }

   initData (): void {
      let here: Cursor = new Cursor(this.tv)
      here
         .to(ExplValue, "t")
         .to(Expl.Defs, "tv")
         .to(ExplValue, "t")
         .to(Expl.Defs, "tv")
         .to(ExplValue, "t")
         .to(Expl.Defs, "def̅")
         .to(Cons, "head")
         .to(Expl.Let, "tv")
      this.data_tv = as(here.v, ExplValue)
   }

   get data (): Data {
      return as(this.data_tv.v as Value, List)
   }

   get graphics (): GraphicsElement {
      return as(this.tv.v as Value, GraphicsElement)
   }
   
   loadExample (): void {
      this.e = parse(load("bar-chart"))
      {
         let here: Cursor = new Cursor(this.e)
         here.skipImports().toDef("data").to(Expr.Let, "e")
         this.data_e = as(here.v, Expr.Constr)
      }
      this.tv = Eval.eval_(emptyEnv(), this.e)
      this.initData()
      setallα(this.e, ann.top)
      Eval.eval_fwd(this.tv)
      this.renderData(this.data)
      this.draw()
   }

   // Push annotations back from data to source, then redo the forward slice.
   redoFwdSlice (): void {
      setallα(this.data_e, ann.bot)
      Eval.eval_bwd(this.data_tv)
      Eval.eval_fwd(this.tv)
      this.draw()
   }

   draw (): void {
      this.dataCtx.clearRect(0, 0, this.dataCanvas.width, this.dataCanvas.height)
      this.dataView.draw()
      this.renderGraphics(this.graphics) // TODO: adopt same "view" pattern?
      // this.graphicsPane3D.render()
   }

   renderData (data: Data): void {
      this.dataCanvas.height = 400
      this.dataCanvas.width = 400
      this.dataView = new DataRenderer(this.dataCtx, data).view
      this.dataCanvas.addEventListener("mousemove", (e: MouseEvent): void => {
         const rect: ClientRect = this.dataCanvas.getBoundingClientRect()
         if (this.dataView.onMouseMove(e.clientX - rect.left, e.clientY - rect.top)) {
            this.redoFwdSlice()
         }
      })
      this.dataCanvas.height = this.dataView.height + 1 // not sure why extra pixel is essential
      this.dataCanvas.width = this.dataView.width
   }

   renderGraphics (g: GraphicsElement): void {
      new GraphicsRenderer(this.graphicsCanvas).render(g)
   }
}

new App()
