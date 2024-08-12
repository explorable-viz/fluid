"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   {},
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
   // console.log("Exercising setSelState")
   const sel = selState(view)
   div
      .classed(selClasses, false)
      .classed(selClassesFor(sel), true)
      .on('mousedown', e => { selListener(e) })
      .on('mouseenter', e => { selListener(e) })
      .on('mouseleave', e => { selListener(e) })
}

function drawLinkedText_ (
   linkedTextHelpers,
   {
      uiHelpers,
      divId,
      suffix,
      view
   },
   selListener
) {
   return () => {
      const div = d3.select('#' + divId)
      let rootElement = div.selectAll('#' + divId)
      div.text(view._1)
      div.attr('class', 'transparent-text')

      setSelState(linkedTextHelpers, uiHelpers, div, view,  selListener)
   } 
}

export var drawLinkedText = x1 => x2 => x3 => drawLinkedText_(x1, x2, x3)