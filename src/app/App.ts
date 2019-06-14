import { ann } from "../util/Annotated"
import { __nonNull, as } from "../util/Core"
import { emptyEnv } from "../Env"
import { Eval } from "../Eval"
import { ExplValue } from "../ExplValue"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { Value } from "../Value"
import { ν, setallα, str } from "../Versioned"
import { importDefaults, load, parse } from "../../test/util/Core"
import { Cursor } from "../../test/util/Cursor"
import { GraphicsRenderer, Slicer, ViewCoordinator, svgNS } from "./GraphicsRenderer"

class View implements Slicer {
   name: string
   coordinator: ViewCoordinator
   e: Expr
   tv: ExplValue
   view: GraphicsRenderer

   constructor (name: string, e: Expr, svg: SVGSVGElement) {
      this.name = name
      this.e = e
      this.tv = Eval.eval_(emptyEnv(), e)
      this.view = new GraphicsRenderer(svg, this)
      this.resetForBwd()
      this.draw()
   }

   fwdSlice (): void {
      console.log(`${this.name}: forward-slicing based on availability annotations.`)
      Eval.eval_fwd(this.tv)
      this.draw()
   }

   resetForBwd (): void {
      console.log(`${this.name}: clearing program annotations and forward-slicing.`)
      setallα(ann.bot, this.e)
      Eval.eval_fwd(this.tv) // clear all annotations
   }

   bwdSlice (): void {
      console.log(`${this.name}: setting neededness annotations and backward-slicing.`)
      Eval.eval_bwd(this.tv)
      this.coordinator.onBwd()
      this.draw()
   }

   get svg (): SVGSVGElement {
      return this.view.ancestors[0] as SVGSVGElement
   }

   getGraphics (): GraphicsElement {
      return as(this.tv.v as Value, GraphicsElement)
   }

   draw (): void {
      this.view.render(this.getGraphics())
   }
}

// "Data" defined to be expression bound by first "let" in user code; must be already in normal form.
class App {
   dataView: View
   graphicsView: View

   constructor () {
      // Two programs share the expression data_e. May be problematic for setting/clearing annotations?
      this.graphicsView = new View(
         "graphicsView",
         parse(load("bar-chart")), 
         this.createSvg(400, 400, false)
      )
      let here: Cursor = new Cursor(this.graphicsView.e)
      here.skipImports().toDef("data").to(Expr.Let, "e")
      const data_e: Expr = as(here.v, Expr.Constr)
      this.dataView = new View(
         "dataView",
         importDefaults(Expr.app(ν(), Expr.var_(ν(), str(ν(), "renderData")), Expr.quote(ν(), data_e))),
         this.createSvg(400, 1200, false)
      )
      const dataView: View = this.dataView
      this.graphicsView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            dataView.fwdSlice()
         }

         resetForBwd (): void {
            dataView.resetForBwd()
            graphicsView.resetForBwd()
         }
      }()
      const graphicsView: View = this.graphicsView
      this.dataView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            graphicsView.fwdSlice()
         }

         resetForBwd (): void {
            dataView.resetForBwd()
            graphicsView.resetForBwd()
         }
      }
      document.body.appendChild(this.dataView.svg)
      document.body.appendChild(this.graphicsView.svg)
   }

   createSvg (w: number, h: number, stackDown: boolean): SVGSVGElement {
      const svg: SVGSVGElement = document.createElementNS(svgNS, "svg")
      svg.setAttribute("width", w.toString())
      svg.setAttribute("height", h.toString())
      // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
      svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`)
      svg.setAttribute("viewBox", `-0.5 ${(stackDown ? -0.5 - h : -0.5).toString()} ${w.toString()} ${h.toString()}`)
      // Don't use SVG transform internally, but compute our own transformations (to avoid having non-integer
      // pixel attributes). But to invert y-axis use an SVG transform:
      svg.setAttribute("transform", "scale(1,-1)")
      svg.style.verticalAlign = "top"
      svg.style.display = "inline-block"
      return svg
   }
}

new App()
