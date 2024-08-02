"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function drawLinkedText_ (
   linkedTextHelpers,
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         contents
      }
   },
   listener
) { }

export var drawLinkedText = x1 => x2 => x3 => drawLinkedText_()