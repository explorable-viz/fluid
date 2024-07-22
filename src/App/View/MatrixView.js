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
      const highlightStrokeWidth = 1.5
      const highlightStrokeColor = 'blue'
      const w = 30, h = 30

      const div = d3.select('#' + divId)
      if (div.empty()) {
         console.error('Unable to insert figure: no div found with id ' + divId)
         return
      }

      const [width, height] = [w * matrix.j + highlightStrokeWidth, h * matrix.i + highlightStrokeWidth]
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
         
         // outer group containing all rows
         const outerGrp = rootElement
            .append('g')
            .attr('transform', `translate(${strokeWidth / 2 + hMargin / 2}, ${strokeWidth / 2 + vMargin})`)            
            // these will be inherited by text elements
            .attr('fill', 'currentColor')
            .attr('stroke', 'currentColor')
            .attr('stroke-width', '.25') // otherwise setting stroke makes it bold
         
         // group for each row
         const grp = outerGrp
            .selectAll('g')
            .data([...matrix.cells.entries()].map(([i, ns]) => { return { i: i + 1, ns } }))
            .enter()
            .append('g')

         const cells = grp
            .selectAll('rect')
            .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j: j + 1, n } }))
            .enter()

         cells.each(function ({i, j, n}) {
            const cell = d3.select(this);
            const x = w * (j - 1);
            const y = h * (i - 1);

            cell.append('rect')
               .classed('matrix-cell', true)
               .attr('x', x)
               .attr('y', y)
               .attr('width', w)
               .attr('height', h)
               .attr('stroke-width', strokeWidth)
            
            cell.append('text')
               .text(val(n))
               .attr('x', (j - 0.5) * w)
               .attr('y', (i - 0.5) * h)
               .attr('class', 'matrix-cell-text')
               .attr('text-anchor', 'middle')
               .attr('dominant-baseline', 'middle')
               .attr('pointer-events', 'none')

            cell.append('line') // top border
               .attr('x1', x)
               .attr('y1', y)
               .attr('x2', x + w)
               .attr('y2', y)
               .attr('stroke-width', highlightStrokeWidth)
               .attr('stroke', highlightStrokeColor);

            cell.append('line') // right border
               .attr('x1', x + w)
               .attr('y1', y)
               .attr('x2', x + w)
               .attr('y2', y + h)
               .attr('stroke-width', highlightStrokeWidth)
               .attr('stroke', highlightStrokeColor);

            cell.append('line') // bottom border
               .attr('x1', x + w)
               .attr('y1', y + h)
               .attr('x2', x)
               .attr('y2', y + h)
               .attr('stroke-width', highlightStrokeWidth)
               .attr('stroke', highlightStrokeColor);

            cell.append('line') // left border
               .attr('x1', x)
               .attr('y1', y + h)
               .attr('x2', x)
               .attr('y2', y)
               .attr('stroke-width', highlightStrokeWidth)
               .attr('stroke', highlightStrokeColor);
         })

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
