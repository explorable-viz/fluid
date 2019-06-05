import { ann } from "../util/Annotated"
import { __nonNull, as } from "../util/Core"
import { emptyEnv } from "../Env"
import { Direction, Eval } from "../Eval"
import { ExplValue } from "../ExplValue"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { unary_, unaryOps } from "../Primitive"
import { Id, Num, Str, Value } from "../Value"
import { Versioned, setallα, numʹ } from "../Versioned"
import { load, parse } from "../../test/util/Core"
import { Cursor } from "../../test/util/Cursor"
import { DataView, DataRenderer } from "./DataRenderer"
import { GraphicsPane3D } from "./GraphicsPane3D"
import { GraphicsRenderer, Slicer, svgNS } from "./GraphicsRenderer"

class App implements Slicer {
   e: Expr                        // entire closed program
   tv: ExplValue                  // chart computed by program
   data_e: Expr                   // expression for data (value bound by first let in user code)
   dataView: DataView
   dataCanvas: HTMLCanvasElement
   dataCtx: CanvasRenderingContext2D
   graphicsView: GraphicsRenderer
   graphicsPane3D: GraphicsPane3D
   svg: SVGSVGElement
   direction: Direction
   
   constructor () {
      this.svg = document.createElementNS(svgNS, "svg")
      this.svg.setAttribute("width", "400")
      this.svg.setAttribute("height", "400")
      // TODO: understand how last two numbers here relate to width and height attributes
      // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
      this.svg.setAttribute("viewBox", "-0.5 -0.5 400 400")
      // We don't use SVG transform internally, but compute our own transformations (to avoid having non-integer
      // pixel attributes). But to invert the y-axis we use an SVG transform:
      this.svg.setAttribute("transform", "scale(1,-1)")
      this.svg.style.verticalAlign = "top"
      this.svg.style.display = "inline-block"

      this.dataCanvas = document.createElement("canvas")
      this.dataCtx = __nonNull(this.dataCanvas.getContext("2d"))
      this.graphicsPane3D = new GraphicsPane3D(600, 600)
      this.dataCanvas.style.verticalAlign = "top"
      this.dataCanvas.style.display = "inline-block"
      this.graphicsPane3D.renderer.domElement.style.verticalAlign = "top"
      this.graphicsPane3D.renderer.domElement.style.display = "inline-block"
      document.body.appendChild(this.dataCanvas)
      document.body.appendChild(this.svg)

      // Additional primitives that rely on offline rendering to compute text metrics.
      const textWidth = (txt: Str) => (k: Id): Versioned<Num> => {
         const text: SVGTextElement = document.createElementNS(svgNS, "text")
         svg.add(text)
         return numʹ(k, text.getBBox().width)
      }
      
      unaryOps.set(textWidth.name, unary_(textWidth))

      // document.body.appendChild(this.graphicsPane3D.renderer.domElement)
      // this.graphicsPane3D.setCanvas(this.graphicsCanvas)
      this.loadExample()
   }

   get graphics (): GraphicsElement {
      return as(this.tv.v as Value, GraphicsElement)
   }
   
   // "Data" is defined to be the expression bound by the first "let" in user code; must be already in normal form.
   initData (): void {
      const here: Cursor = new Cursor(this.e)
      here.skipImports().toDef("data").to(Expr.Let, "e")
      this.data_e = as(here.v, Expr.Constr)
   }

   loadExample (): void {
      this.e = parse(load("bar-chart"))
      this.tv = Eval.eval_(emptyEnv(), this.e)
      this.initData()
      this.renderData(this.data_e)
      this.graphicsView = new GraphicsRenderer(this.svg, this)
      this.resetForFwd()
      this.fwdSlice()
   }

   resetForFwd (): void {
      setallα(ann.top, this.e)
   }

   fwdSlice (): void {
      Eval.eval_fwd(this.tv)
      this.direction = Direction.Fwd
      this.draw()
   }

   resetForBwd (): void {
      setallα(ann.bot, this.e)
      Eval.eval_fwd(this.tv) // to clear all annotations
   }

   bwdSlice (): void {
      Eval.eval_bwd(this.tv)
      this.direction = Direction.Bwd
      this.draw()
   }

   draw (): void {
      this.dataCtx.clearRect(0, 0, this.dataCanvas.width, this.dataCanvas.height)
      this.dataView.draw()
      this.graphicsView.render(this.graphics)
      // this.graphicsPane3D.render()
   }

   renderData (data: Expr): void {
      this.dataCanvas.height = 400
      this.dataCanvas.width = 400
      this.dataView = new DataRenderer(this.dataCtx, data, this).view
      this.dataCanvas.addEventListener("mousemove", (e: MouseEvent): void => {
         const rect: ClientRect = this.dataCanvas.getBoundingClientRect()
         this.resetForFwd()
         if (this.dataView.onMouseMove(e.clientX - rect.left, e.clientY - rect.top)) {
            this.fwdSlice()
         }
      })
      this.dataCanvas.height = this.dataView.height + 1 // why extra pixel needed?
      this.dataCanvas.width = this.dataView.width
   }
}

new App()
