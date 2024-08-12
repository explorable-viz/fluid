"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   {
      selState,
      selClasses,
      selClassesFor,
      join
   },
   rootElement,
   listener
) {
   
}

function drawLinkedText_ (
   linkedTextHelpers,
   {
      uiHelpers,
      divId,
      suffix,
      view
   },
   listener
) {
   return () => {
      // const { val } = uiHelpers
      const div = d3.select('#' + divId)
      console.log("Lol")
      console.log(view)
      document.getElementById(divId).innerText = view._1
   } 
}

export var drawLinkedText = x1 => x2 => x3 => drawLinkedText_(x1, x2, x3)