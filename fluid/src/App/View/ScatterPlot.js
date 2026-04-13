"use strict"

import * as d3 from "d3"

function renderAxes_ (scales, width, height, rootElement) {
   return () => {
      rootElement.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(scales.x).tickSizeOuter(0))
         .selectAll('text')
         .style('text-anchor', 'middle')
      rootElement.append('g')
         .call(d3.axisLeft(scales.y).tickSizeOuter(0))
   }
}

export var renderAxes = x1 => x2 => x3 => x4 => renderAxes_(x1, x2, x3, x4)
