import { __nonNull } from "../util/Core"
import { ann } from "../util/Lattice"
import { setallα } from "../Annotated"
import { Expl_ } from "../DataValue"
import { Direction, Eval } from "../Eval"
import { Expr } from "../Expr"
import   { GraphicsElement } from "../Graphics"
import { Dataset, module_graphics, module_renderData, openWithImports, openDatasetAs, parseWithImports } from "../Module"
import {clearMemo } from "../Value"
import { GraphicsRenderer, Slicer, ViewCoordinator, svgNS } from "./GraphicsRenderer"

export class View implements Slicer {
   name: string
   coordinator: ViewCoordinator
   e: Expr
   tv: Expl_
   view: GraphicsRenderer
   direction: Direction

   constructor (name: string, dataset: Dataset, e: Expr, svg: SVGSVGElement) {
      this.name = name
      this.e = e
      this.tv = Eval.eval_(dataset.ρ, e)
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
      this.direction = Direction.Bwd
      this.coordinator.onBwd()
      this.draw()
   }

   get svg (): SVGSVGElement {
      return this.view.ancestors[0] as SVGSVGElement
   }

   getGraphics (): Expl_<GraphicsElement> {
      return this.tv as Expl_<GraphicsElement>
   }

   draw (): void {
      this.view.render(this.getGraphics())
   }
}

class App {
   dataView: View
   graphicsView: View

   constructor () {
      // data has recursive type Data = Versioned<List<Pair<Num | Str, Data>>>
      const dataset: Dataset = openDatasetAs("renewables", "data")
      clearMemo()
      dataset.setallα(ann.top)
      this.graphicsView = new View(
         "graphicsView", 
         dataset,
         openWithImports("bar-chart", [module_graphics]), 
         this.createSvg(400, 400, false)
      )
      this.dataView = new View(
         "dataView", 
         dataset,
         parseWithImports("renderData data", [module_graphics, module_renderData]), 
         this.createSvg(400, 1200, false)
      )
      const dataView: View = this.dataView
      this.graphicsView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            clearMemo()
            dataset.negateallα()
            dataView.fwdSlice()
         }

         resetForBwd (): void {
            clearMemo()
            dataset.setallα(ann.bot)
            dataView.resetForBwd()
            graphicsView.resetForBwd()
         }
      }()
      const graphicsView: View = this.graphicsView
      this.dataView.coordinator = new class ViewCoordinator {
         onBwd (): void {
            clearMemo()
            dataset.negateallα()
            graphicsView.fwdSlice()
         }

         resetForBwd (): void {
            clearMemo()
            dataset.setallα(ann.bot)
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
