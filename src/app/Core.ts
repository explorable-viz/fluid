// First-class moodule.
export class SVG {
   static NS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"
   invert_y: boolean
   private metrics: SVGSVGElement

   constructor (invert_y: boolean) {
      this.invert_y = invert_y
      this.metrics = document.createElementNS(SVG.NS, "svg")
      this.metrics.setAttribute("width", "0")
      this.metrics.setAttribute("height", "0")
      this.metrics.style.visibility = "hidden"
      document.body.appendChild(this.metrics)
   }

   createSvg (w: number, h: number): SVGSVGElement {
      const svg: SVGSVGElement = document.createElementNS(SVG.NS, "svg")
      svg.setAttribute("width", w.toString())
      svg.setAttribute("height", h.toString())
      // See https://vecta.io/blog/guide-to-getting-sharp-and-crisp-svg-images
      svg.setAttribute("viewBox", `-0.5 -0.5 ${w.toString()} ${h.toString()}`)
      // Don't use SVG transform internally, but compute our own transformations (to avoid having non-integer
      // pixel attributes). But to invert y-axis use an SVG transform:
      if (this.invert_y) {
         svg.setAttribute("transform", "scale(1,-1)")
      }
      svg.style.verticalAlign = "top"
      svg.style.display = "inline-block"
      const defs: SVGDefsElement = document.createElementNS(SVG.NS, "defs")
      const marker: SVGMarkerElement = document.createElementNS(SVG.NS, "marker")
      marker.setAttribute("id", "arrowhead")
      marker.setAttribute("refX", "0")
      marker.setAttribute("refY", "5")
      marker.setAttribute("markerUnits", "strokeWidth")
      marker.setAttribute("markerWidth", "4")
      marker.setAttribute("markerHeight", "3")
      marker.setAttribute("orient", "auto")
      const path: SVGPathElement = document.createElementNS(SVG.NS, "path")
      marker.appendChild(path)
      path.setAttribute("d", "M 0 0 L 10 5 L 0 10 z")
      defs.appendChild(marker)
      svg.appendChild(defs)
      return svg
   }

   textWidth (text: SVGTextElement): number {
      this.metrics.appendChild(text)
      const width: number = text.getBBox().width
      text.remove()
      return width
   }

   textHeight (text: SVGTextElement): number {
      this.metrics.appendChild(text)
      const height: number = text.getBBox().height
      text.remove()
      return height
   }
}
