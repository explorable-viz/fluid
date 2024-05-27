"use strict"

import * as d3 from "d3"

function setSelState (
   {
      selState,
      selClasses,
      selClass
   },
   rootElement,
   { matrix },
   listener
) {
   rootElement.selectAll('.matrix-cell').each(function (cell) {
      const sel = selState(matrix.cells[cell.i - 1][cell.j - 1])
      d3.select(this) // won't work inside arrow function :/
         .classed(selClasses, false)
         .classed(selClass(sel), true)
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
}

function drawMatrix_ (
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         title,    // String
         matrix    // IntMatrix
      }
   },
   listener
) {
   return () => {
      const { val } = uiHelpers
      const childId = divId + '-' + suffix
      const strokeWidth = 0.5
      const w = 30, h = 30
      const div = d3.select('#' + divId)
      const [width, height] = [w * matrix.j + strokeWidth, h * matrix.i + strokeWidth]
      const hMargin = w / 2
      const vMargin = h / 2

      let rootElement = div.selectAll('#' + childId)

      if (rootElement.empty()) {
         rootElement = div
            .append('svg')
            .attr('id', childId)

         rootElement
            .attr('width', width + hMargin)
            .attr('height', height + vMargin)

         // group for each row
         const grp = rootElement
            .selectAll('g')
            .data([...matrix.cells.entries()].map(([i, ns]) => { return { i: i + 1, ns } }))
            .enter()
            .append('g')
            .attr(
               'transform',
               (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`
            )

         const rect = grp
            .selectAll('rect')
            .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j: j + 1, n } }))
            .enter()

         rect
            .append('rect')
            .classed('matrix-cell', true)
            .attr('x', (_, j) => w * j)
            .attr('width', w)
            .attr('height', h)
            .attr('stroke-width', strokeWidth)

         rect
            .append('text')
            .text(({ n }) => val(n))
            .attr('x', (_, j) => w * (j + 0.5))
            .attr('y', 0.5 * h)
            .attr('class', 'matrix-cell-text')
            .attr('text-anchor', 'middle')
            .attr('dominant-baseline', 'middle')
            .attr('pointer-events', 'none')

         rootElement.append('text')
            .text(title)
            .attr('x', hMargin / 2)
            .attr('y', vMargin / 2)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'middle')
            .attr('text-anchor', 'left')
      }
      setSelState(uiHelpers, rootElement, { matrix }, listener)
   }
}

export var drawMatrix = x1 => x2 => drawMatrix_(x1, x2)
