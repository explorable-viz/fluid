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
   console.log("Exercising setSelState")
   div.selectAll('span').each(function (content) {
      console.log("Entered selection in setSelState")
      console.log(content)
      const sel = selState(content)
      console.log(sel)
      d3.select(this)
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
         .on('mousedown', e => { selListener(e) })
         .on('mouseenter', e => { selListener(e) })
         .on('mouseleave', e => { selListener(e) })
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
      console.log(view)
      const div = d3.select('#' + divId)
      const childId = divId + '-' + suffix
      let rootElement = div.selectAll('#' + childId)
      if (rootElement.empty()) {
         console.log("Root element empty!")
         console.log(rootElement)
         console.log(view.entries())
         rootElement = div.append("div").attr("id", childId).text(view._1).attr('class', 'transparent-text-parent')
         

         const textElems = rootElement
                           .selectAll('span')
                           .data(view)
                           .enter()
                           .append('span')
                           .attr('id', childId)
                           .text(d => d._1)
                           .attr('class', 'transparent-text')
      }

      setSelState(linkedTextHelpers, uiHelpers, rootElement, view,  selListener)
   } 
}

export var drawLinkedText = x1 => x2 => x3 => x4 => drawLinkedText_(x1, x2, x3, x4)
