import { __nonNull } from "../util/Core"
import { ann } from "../util/Lattice"
import { setallα, negateallα } from "../Annotated"
import { ExplValue } from "../DataValue"
import { __delta, clearDelta } from "../Delta"
import { Env } from "../Env"
import { Direction, Eval } from "../Eval"
import { Expr } from "../Expr"
import  { GraphicsElement } from "../Graphics"
import { module_graphics, module_renderData, openWithImports, openDatasetAs, parseWithImports } from "../Module"
import { clearMemo } from "../Value"
import { GraphicsRenderer, Slicer, ViewCoordinator, svgNS } from "./GraphicsRenderer"

// As with the test cases, we treat the dataset ρ as "external" data, meaning we push slicing
// information back only as far as ρ.
export class View implements Slicer {
   name: string
   coordinator!: ViewCoordinator
   e: Expr
   tv: ExplValue
   view: GraphicsRenderer
   direction!: Direction

   constructor (name: string, ρ: Env, e: Expr, svg: SVGSVGElement) {
      this.name = name
      this.e = e
      this.tv = Eval.eval_(ρ, e)
      this.view = new GraphicsRenderer(svg, this)
      this.fwdSlice()
      this.draw()
   }

   // Consider (un)availability of dataset only; treat e as an unlimited resource.
   fwdSlice (): void {
      setallα(ann.top, this.e)
      Eval.eval_fwd(this.e, this.tv)
      this.direction = Direction.Fwd
      this.draw()
   }
   
   // Clear annotations on program and forward slice, to erase all annotations prior to backward slicing.
   resetForBwd (): void {
      setallα(ann.bot, this.e)
      Eval.eval_fwd(this.e, this.tv)
   }

   bwdSlice (): void {
      Eval.eval_bwd(this.e, this.tv)
      clearDelta()
      console.log(__delta.size)
      this.direction = Direction.Bwd
      this.coordinator.onBwd()
      this.draw()
   }

   get svg (): SVGSVGElement {
      return this.view.ancestors[0] as SVGSVGElement
   }

   getGraphics (): ExplValue<GraphicsElement> {
      return this.tv as ExplValue<GraphicsElement>
   }

   draw (): void {
      this.view.render(this.getGraphics())
   }
}

// Data has approximate recursive type
// Data = Versioned<List<Pair<Num | Str, Data>>>

class App {
   dataView: View
   graphicsView: View

   constructor () {
      const ρ: Env = openDatasetAs("renewables", "data")
      clearMemo()
      setallα(ann.top, ρ)
      this.graphicsView = new View(
         "graphicsView", 
         ρ,
         openWithImports("bar-chart", [module_graphics]), 
         this.createSvg(400, 400, false)
      )
      this.dataView = new View(
         "dataView", 
         ρ,
         parseWithImports("renderData data", [module_graphics, module_renderData]), 
         this.createSvg(400, 1200, false)
      )
      const dataView: View = this.dataView
      this.graphicsView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            clearMemo()
            negateallα(ρ)
            dataView.fwdSlice()
         }

         resetForBwd (): void {
            clearMemo()
            setallα(ann.bot, ρ)
            dataView.resetForBwd()
            graphicsView.resetForBwd()
         }
      }()
      const graphicsView: View = this.graphicsView
      this.dataView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            clearMemo()
            negateallα(ρ)
            graphicsView.fwdSlice()
         }

         resetForBwd (): void {
            clearMemo()
            setallα(ann.bot, ρ)
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
