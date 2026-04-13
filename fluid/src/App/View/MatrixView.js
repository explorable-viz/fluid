"use strict"

import * as d3 from "d3"

const w = 30, h = 30
const strokeWidth = 0.5
const highlightStrokeWidth = 0.5
const highlightStrokeColor = 'blue'

function createBorders_ (matrix, rootElement) {
   return () => {
      const hMargin = w / 2
      const vMargin = h / 2

      const bordersGrp = rootElement
         .append('g')
         .attr('transform', `translate(${highlightStrokeWidth / 2 + hMargin / 2}, ${highlightStrokeWidth / 2 + vMargin})`)
         .attr('fill', 'currentColor')
         .attr('stroke', highlightStrokeColor)
         .attr('stroke-width', highlightStrokeWidth)

      const hBordersGrp = bordersGrp.append('g')

      hBordersGrp
         .selectAll('g')
         .data(d3.range(matrix.i + 1))
         .enter()
         .append('g')
         .each(function(d) {
            d3.select(this)
               .selectAll('line')
               .data(d3.range(1, matrix.j + 1).map(j => ({ i: d, j })), d => d.j)
               .enter()
               .append('line')
               .attr('x1', ({j}) => (j - 1) * w)
               .attr('y1', ({i}) => i * h)
               .attr('x2', ({j}) => j * w)
               .attr('y2', ({i}) => i * h)
               .attr('class', 'matrix-cell-hBorder')
         });

      const vBordersGrp = bordersGrp.append('g')

      vBordersGrp
         .selectAll('g')
         .data(d3.range(1, matrix.i + 1), i => i)
         .enter()
         .append('g')
         .each(function(d) {
            d3.select(this)
               .selectAll('line')
               .data(d3.range(matrix.j + 1).map(j => ({ i: d, j })))
               .enter()
               .append('line')
               .attr('x1', ({j}) => j * w)
               .attr('y1', ({i}) => (i - 1) * h)
               .attr('x2', ({j}) => j * w)
               .attr('y2', ({i}) => i * h)
               .attr('class', 'matrix-cell-vBorder')
         });
   }
}

export var createBorders = x1 => x2 => createBorders_(x1, x2)
