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
