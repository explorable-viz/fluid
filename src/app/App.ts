import { ann } from "../util/Annotated"
import { __nonNull, as } from "../util/Core"
import { List, Pair } from "../BaseTypes"
import { Env, emptyEnv, extendEnv } from "../Env"
import { Eval } from "../Eval"
import { ExplValue } from "../ExplValue"
import { Expr } from "../Expr"
import { GraphicsElement } from "../Graphics"
import { open, openDatasetAs, parseWithImports } from "../Module"
import { Num, Str, Value } from "../Value"
import { Versioned, ν, setallα, str } from "../Versioned"
import { GraphicsRenderer, Slicer, ViewCoordinator, svgNS } from "./GraphicsRenderer"

class View implements Slicer {
   name: string
   coordinator: ViewCoordinator
   ρ: Env
   e: Expr
   tv: ExplValue
   view: GraphicsRenderer

   constructor (name: string, ρ: Env, e: Expr, svg: SVGSVGElement) {
      this.name = name
      this.e = e
      this.tv = Eval.eval_(ρ, e)
      this.view = new GraphicsRenderer(svg, this)
      this.resetForBwd()
      this.draw()
   }

   fwdSlice (): void {
      setallα(ann.top, this.e)
      Eval.eval_fwd(this.tv)
      this.draw()
   }

   // Clear annotations on program and forward slice, to erase all annotations prior to backward slicing.
   resetForBwd (): void {
      setallα(ann.bot, this.e)
      Eval.eval_fwd(this.tv)
   }

   bwdSlice (): void {
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

type Data = Versioned<List<Pair<Num | Str, any>>> // approximate recursive type

class App {
   dataView: View
   graphicsView: View

   constructor () {
      const ρ: Env = openDatasetAs("renewables"),
            data: Data = Eval.eval_(emptyEnv(), ).v as Data
      setallα(ann.bot, data)
      this.graphicsView = new View("graphicsView", ρ, open("bar-chart"), this.createSvg(400, 400, false))
      this.dataView = new View("dataView", ρ, parseWithImports("renderData data", ["renderData"]), this.createSvg(400, 1200, false))
      const dataView: View = this.dataView
      this.graphicsView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            dataView.fwdSlice()
         }

         resetForBwd (): void {
            setallα(ann.bot, data)
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
            setallα(ann.bot, data)
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
