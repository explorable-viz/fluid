// First-class module.
export class SVG {
   static NS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"
   private hiddenMetricsElement: SVGSVGElement

   constructor () {
      this.hiddenMetricsElement = document.createElementNS(SVG.NS, "svg")
      this.hiddenMetricsElement.setAttribute("width", "0")
      this.hiddenMetricsElement.setAttribute("height", "0")
      this.hiddenMetricsElement.style.visibility = "hidden"
      document.body.appendChild(this.hiddenMetricsElement)
   }

   textWidth (text: SVGTextElement): number {
      this.hiddenMetricsElement.appendChild(text)
      const width: number = text.getBBox().width
      text.remove()
      return width
   }

   textHeight (text: SVGTextElement): number {
      this.hiddenMetricsElement.appendChild(text)
      const height: number = text.getBBox().height
      text.remove()
      return height
   }
}
