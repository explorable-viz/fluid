import { ann } from "../util/Annotated"
import { __nonNull, as } from "../util/Core"
import { emptyEnv, extendEnv } from "../Env"
import { Direction, Eval } from "../Eval"
import { Expl, ExplValue } from "../ExplValue"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { Value } from "../Value"
import { ν, setallα, str } from "../Versioned"
import { importDefaults, load, parse } from "../../test/util/Core"
import { Cursor } from "../../test/util/Cursor"
import { DataView, DataRenderer } from "./DataRenderer"
import { GraphicsRenderer, Slicer, svgNS } from "./GraphicsRenderer"

class App implements Slicer {
   e: Expr                        // entire closed program
   tv: ExplValue                  // chart computed by program
   data_e: Expr                   // expression for data (value bound by first let in user code)
   data_tv: ExplValue
   dataView: DataView
   dataView2: GraphicsRenderer
   dataView2_tv: ExplValue
   dataCanvas: HTMLCanvasElement
   dataSvg: SVGSVGElement
   dataCtx: CanvasRenderingContext2D
   graphicsView: GraphicsRenderer
   graphicsSvg: SVGSVGElement
   direction: Direction

   constructor () {
      this.graphicsSvg = this.createSvg(400, 400)
      this.dataSvg = this.createSvg(400, 400)
      this.dataCanvas = document.createElement("canvas")
      this.dataCtx = __nonNull(this.dataCanvas.getContext("2d"))
      this.dataCanvas.style.verticalAlign = "top"
      this.dataCanvas.style.display = "inline-block"
      document.body.appendChild(this.dataCanvas)
      document.body.appendChild(this.dataSvg)
      document.body.appendChild(this.graphicsSvg)
      this.loadExample()
   }

   createSvg (h: number, w: number): SVGSVGElement {
      const svg: SVGSVGElement = document.createElementNS(svgNS, "svg")
      svg.setAttribute("width", w.toString())
      svg.setAttribute("height", h.toString())
      // TODO: understand how last two numbers below relate to width and height attributes above.
      // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
      svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`)
      // We don't use SVG transform internally, but compute our own transformations (to avoid having non-integer
      // pixel attributes). But to invert the y-axis we use an SVG transform:
      svg.setAttribute("transform", "scale(1,-1)")
      svg.style.verticalAlign = "top"
      svg.style.display = "inline-block"
      return svg
   }

   getGraphics (): GraphicsElement {
      return as(this.tv.v as Value, GraphicsElement)
   }

   getDataGraphics(): GraphicsElement {
      return as(this.dataView2_tv.v as Value, GraphicsElement)
   }
   
   // "Data" is defined to be the expression bound by the first "let" in user code; must be already in normal form.
   initData (): void {
      let here: Cursor = new Cursor(this.e)
      here.skipImports().toDef("data").to(Expr.Let, "e")
      this.data_e = as(here.v, Expr.Constr)

      here = new Cursor(this.tv)
      here
         .to(ExplValue, "t")
         .to(Expl.Defs, "tv")
         .to(ExplValue, "t")
         .to(Expl.Defs, "tv")
         .to(ExplValue, "t")
         .to(Expl.Defs, "tv")
         .to(ExplValue, "t")
         .to(Expl.Defs, "def̅")
         .toElem(0)
         .assert(Expl.Let, tv => tv.x.val === "data")
         .to(Expl.Let, "tv")
      this.data_tv = as(here.v, ExplValue)
   }

   // TODO: sharing of data_e is not nice, and probably problematic w.r.t. set/clearing annotations.
   initDataView (data_e: Expr): void {
      const e: Expr = importDefaults(Expr.app(ν(), Expr.var_(ν(), str(ν(), "renderData")), Expr.var_(ν(), str(ν(), "data"))))
      this.dataView2_tv = Eval.eval_(extendEnv(emptyEnv(), str(ν(), "data"), this.data_tv.v), e)
      setallα(ann.top, e)
      Eval.eval_fwd(this.dataView2_tv)
   }

   loadExample (): void {
      this.e = parse(load("bar-chart"))
      this.tv = Eval.eval_(emptyEnv(), this.e)
      this.initData()
      this.renderData(this.data_e)
      this.initDataView(this.data_e)
      this.dataView2 = new GraphicsRenderer(this.dataSvg, this)
      this.graphicsView = new GraphicsRenderer(this.graphicsSvg, this)
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
      this.dataView2.render(this.getDataGraphics())
      this.graphicsView.render(this.getGraphics())
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
