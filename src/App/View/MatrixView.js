"use strict"

import * as d3 from "d3"

function setSelState (
   { cell_attrs },
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
         .attrs(cell_attrs(matrix))
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
      const highlightStrokeWidth = 0.5
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
         
         // group for the whole matrix 
         const matrixGrp = rootElement
            .append('g')
            .attr('transform', `translate(${highlightStrokeWidth / 2 + hMargin / 2}, ${highlightStrokeWidth / 2 + vMargin})`)            
            // these will be inherited by text elements
            .attr('fill', 'currentColor')
            .attr('stroke', 'currentColor')
            .attr('stroke-width', '.25') // otherwise setting stroke makes it bold

         // group for each row
         const rowGrp = matrixGrp
            .selectAll('g')
            .data([...matrix.cells.entries()].map(([i, ns]) => { return { i: i + 1, ns } }))
            .enter()
            .append('g')

         // group for cell elements
         const cellGrp = rowGrp
            .selectAll('g')
            .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j: j + 1, n } }))
            .enter()
            .append('g')
            .classed('matrix-cell', true)

         cellGrp.each(function ({i, j, n}) {
            const cell = d3.select(this);
            const x = w * (j - 1);
            const y = h * (i - 1);

            cell.append('rect')
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
      setSelState(matrixHelpers, uiHelpers, rootElement, { matrix }, listener)
   }
}

export var drawMatrix = x1 => x2 => x3 => drawMatrix_(x1, x2, x3)
