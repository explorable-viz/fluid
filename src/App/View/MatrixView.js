"use strict"

import * as d3 from "d3"

function setSelState (
   {
      selState,
      selClasses,
      selClassesFor
   },
   rootElement,
   { matrix },
   listener
) {
   rootElement.selectAll('.matrix-cell').each(function (cell) {
      const sel = selState(matrix.cells[cell.i - 1][cell.j - 1])
      d3.select(this) // won't work inside arrow function :/
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
   rootElement.selectAll('.matrix-cell-text').each(function (cell) {
      const sel = selState(matrix.cells[cell.i - 1][cell.j - 1])
      d3.select(this) // won't work inside arrow function :/
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
   })
}

function drawMatrix_ (
   matrixHelpers,
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
      if (div.empty()) {
         console.error('Unable to insert figure: no div found with id ' + divId)
         return
      }

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
               _ => `translate(${strokeWidth / 2 + hMargin / 2}, ${strokeWidth / 2 + vMargin})`
            )
            // these will be inherited by text elements
            .attr('fill', 'currentColor')
            .attr('stroke', 'currentColor')
            .attr('stroke-width', '.25') // otherwise setting stroke makes it bold

         const cells = grp
            .selectAll('rect')
            .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j: j + 1, n } }))
            .enter()

         cells
            .append('rect')
            .classed('matrix-cell', true)
            .attr('x', ({j}) => w * (j - 1))
            .attr('y', ({i}) => h * (i - 1))
            .attr('width', w)
            .attr('height', h)
            .attr('stroke-width', strokeWidth)

         cells
            .append('text')
            .text(({ n }) => val(n))
            .attr('x', ({j}) => (j - 0.5) * w)
            .attr('y', ({i}) => (i - 0.5) * h)
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

export var drawMatrix = x1 => x2 => x3 => drawMatrix_(x1, x2, x3)
