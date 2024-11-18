"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   { },
   {
      selState,
      selClasses,
      selClassesFor,
      join
   },
   div,
   tooltip,
   view,
   selListener
) {
   div.selectAll('span').each(function (textElem) {
      const sel = selState(view[textElem.i])
      d3.select(this)
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
         .on('mousedown', e => { selListener(e) })
         .on('mouseover', e => { tooltip.style("opacity", 1).html("The exact value of this cell is TODO").style("left", (d3.mouse(this)[0]+70)+"px").style("top", (d3.mouse(this)[1]) + "px")})
         .on('mouseenter', e =>{ selListener(e)})
         .on('mouseleave', e => { selListener(e)})
         .on("mouseout", e => { tooltip.style("opacity", 0)})
   })
}

function drawLinkedText_ (
   linkedTextHelpers,
   uiHelpers,
   {
      divId,
      suffix,
      view
   },
   selListener
) {
   return () => {
      const div = d3.select('#' + divId)
      const childId = divId + '-' + suffix
      let rootElement = div.selectAll('#' + childId)
      var Tooltip
      if (rootElement.empty()) {
         rootElement = div
            .append("div")
            .attr("id", childId)
            .text(view._1)
            .attr('class', 'linked-text-parent')

         rootElement.selectAll('span')
            .data([...view.entries()].map(([i, conts]) => { return {i, conts}}))
            .enter()
            .append('span')
            .attr('id', childId)
            .text(d => d.conts._1)
            .attr('class', 'linked-text')

         Tooltip = div
            .append("div")
            .style("opacity", 0)
            .attr("class", "tooltip")
            .style("background-color", "white")
            .style("border", "solid")
            .style("border-color", "#40BFA0")
            .style("border-width", "2px")
            .style("border-radius", "5px")
            .style("padding", "5px")
      }
      setSelState(linkedTextHelpers, uiHelpers, rootElement, Tooltip, view, selListener)
   }
}

export var drawLinkedText = x1 => x2 => x3 => x4 => drawLinkedText_(x1, x2, x3, x4)
