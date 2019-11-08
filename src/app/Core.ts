// First-class module.
export class SVG {
   static NS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"
   private metrics: SVGSVGElement

   constructor () {
      this.metrics = document.createElementNS(SVG.NS, "svg")
      this.metrics.setAttribute("width", "0")
      this.metrics.setAttribute("height", "0")
      this.metrics.style.visibility = "hidden"
      document.body.appendChild(this.metrics)
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
