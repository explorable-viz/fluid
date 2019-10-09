export const svgNS: "http://www.w3.org/2000/svg" = "http://www.w3.org/2000/svg"

export function createSvg (w: number, h: number, stackDown: boolean): SVGSVGElement {
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

// The SVG text element for the supplied text; centralised so can be used to compute text metrics.
// Use "translate" to locate the element, so that we can apply it after scaling.
export function textElement (x: number, y: number, fontSize: number, str: string): SVGTextElement {
   const text: SVGTextElement = document.createElementNS(svgNS, "text")
   text.setAttribute("stroke", "none")
   text.setAttribute("font-size", fontSize.toString())
   text.setAttribute("transform", `translate(${x.toString()},${y.toString()}) scale(1,-1)`)
   text.appendChild(document.createTextNode(str))
   return text
}

export function textWidth (fontSize: number, str: string): number {
   const text: SVGTextElement = textElement(0, 0, fontSize, str)
   svgMetrics.appendChild(text)
   const width: number = text.getBBox().width
   text.remove()
   return width
}

export function textHeight (fontSize: number, str: string): number {
   const text: SVGTextElement = textElement(0, 0, fontSize, str)
   svgMetrics.appendChild(text)
   const height: number = text.getBBox().height
   return height
}

export let svgMetrics: SVGSVGElement

{
   svgMetrics = document.createElementNS(svgNS, "svg")
   svgMetrics.setAttribute("width", "0")
   svgMetrics.setAttribute("height", "0")
//   svgMetrics.style.visibility = "hidden"
   document.body.appendChild(svgMetrics)
}
