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
   view,
   selListener
) {
   div.selectAll('span').each(function (textElem) {
      const sel = selState(view[textElem.i])
      d3.select(this)
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
         .on('mousedown', e => { selListener(e) })
         .on('mouseenter', e =>{ selListener(e)})
         .on('mouseleave', e => { selListener(e)})
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
      if (rootElement.empty()) {
         rootElement = div
            .append("div")
            .attr("id", childId)
            .text(view._1)
            .attr('class', 'linked-text-parent')
         var tooltip = div
            .append("div")
            .style("opacity", 0)
            .attr("class", "tooltip")
            .style("position", "absolute")
            .style("background-color", "white")
            .style("border", "solid")
            .style("border-color", "#40BFA0")
            .style("border-width", "2px")
            .style("border-radius", "5px")
            .style("padding", "5px")
         rootElement.selectAll('span')
            .data([...view.entries()].map(([i, conts]) => { console.log(JSON.stringify(conts)); return {i, conts}}))
            .enter()
            .append('span')
            .attr('id', childId)
            .text(d => d.conts._1._1)
            .attr('class', 'linked-text')
            .on("mousemove", (e, d) => { tooltip.html("The exact value of this cell is " + d.conts._1).style("left", (e.pageX+70)+"px").style("top", e.pageY + "px") })
            .on("mouseover", (e, d)=> { tooltip.style("opacity", 1)} )
            .on("mouseout", d=> { tooltip.style("opacity", 0)})
      }
      setSelState(linkedTextHelpers, uiHelpers, rootElement, view, selListener)
   }
}

export var drawLinkedText = x1 => x2 => x3 => x4 => drawLinkedText_(x1, x2, x3, x4)
