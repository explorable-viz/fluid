"use strict"

import * as d3 from "d3"

function createAxes_ (xDomain, yDomain, width, height, rootElement) {
   return () => {
      const x = d3.scaleLinear()
         .domain(xDomain)
         .range([0, width])
      rootElement.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x).tickSizeOuter(0))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain(yDomain)
         .range([height, 0])
      rootElement.append('g')
         .call(d3.axisLeft(y).tickSizeOuter(0))

      return { x, y }
   }
}

export var createAxes = x1 => x2 => x3 => x4 => x5 => createAxes_(x1, x2, x3, x4, x5)
