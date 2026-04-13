"use strict"

import * as d3 from "d3"

const w = 30, h = 30
const strokeWidth = 0.5
const highlightStrokeWidth = 0.5
const highlightStrokeColor = 'blue'

function createCells_ ({ val }, matrix, rootElement) {
   return () => {
      const hMargin = w / 2
      const vMargin = h / 2

      const matrixGrp = rootElement
         .append('g')
         .attr('transform', `translate(${highlightStrokeWidth / 2 + hMargin / 2}, ${highlightStrokeWidth / 2 + vMargin})`)
         .attr('fill', 'currentColor')
         .attr('stroke', 'currentColor')
         .attr('stroke-width', '.25')

      const rowGrp = matrixGrp
         .selectAll('g')
         .data([...matrix.cells.entries()].map(([i, ns]) => { return { i, ns } }))
         .enter()
         .append('g')

      const cells = rowGrp
         .selectAll('g')
         .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j, n } }))
         .enter()

      cells
         .append('rect')
         .attr('x', ({j}) => j * w)
         .attr('y', ({i}) => i * h)
         .attr('width', w)
         .attr('height', h)
         .attr('class', 'matrix-cell')
         .attr('stroke-width', strokeWidth)

      cells
         .append('text')
         .text(({n}) => val(n))
         .attr('x', ({j}) => (j + 0.5) * w)
         .attr('y', ({i}) => (i + 0.5) * h)
         .attr('class', 'matrix-cell-text')
         .attr('text-anchor', 'middle')
         .attr('dominant-baseline', 'middle')
         .attr('pointer-events', 'none')
   }
}

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

export var createCells = x1 => x2 => x3 => createCells_(x1, x2, x3)
export var createBorders = x1 => x2 => createBorders_(x1, x2)
