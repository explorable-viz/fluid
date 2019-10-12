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
      return svg
   }

   // The SVG text element for the supplied text; centralised so can be used to compute text metrics.
   // Use "translate" to locate the element, so that we can apply it after scaling.
   textElement (x: number, y: number, fontSize: number, class_: string, str: string): SVGTextElement {
      const text: SVGTextElement = document.createElementNS(SVG.NS, "text")
      text.setAttribute("stroke", "none")
      text.setAttribute("font-size", fontSize.toString()) // wasn't able to set this through CSS for some reason
      let transform: string = `translate(${x.toString()},${y.toString()})`
      if (this.invert_y) {
         transform += " scale(1,-1)"
      }
      text.setAttribute("transform", transform)
      text.setAttribute("class", class_) // set styling before creating text node, for font metrics to be correct
      text.appendChild(document.createTextNode(str))
      return text
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
