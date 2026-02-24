"use strict"

import * as d3 from "d3"

function setSelection_ (
   {
      hBorderStyles,
      vBorderStyles,
      eventListener,
      withElement
   },
   {
      selState,
      selClasses,
      selClassesFor
   },
   { matrix },
   select,
   rootElement
) {
   return () => {
      var listener = eventListener(withElement(select))()
      rootElement.selectAll('.matrix-cell').each(function (cellRect) {
         const sel = selState(matrix.cells[cellRect.i - 1][cellRect.j - 1])
         d3.select(this) // won't work inside arrow function :/
            .classed(selClasses, false)
            .classed(selClassesFor(sel), true)
            .on('mousedown', e => { listener(e) })
            .on('mouseenter', e => { listener(e) })
            .on('mouseleave', e => { listener(e) })
      })

      rootElement.selectAll('.matrix-cell-text').each(function (cellText) {
         const sel = selState(matrix.cells[cellText.i - 1][cellText.j - 1])
         d3.select(this) // won't work inside arrow function :/
            .classed(selClasses, false)
            .classed(selClassesFor(sel), true)
      })

      rootElement.selectAll('.matrix-cell-hBorder').each(function (hBorder) {
         d3.select(this)
            .attr('style', hBorderStyles(matrix)(hBorder))
      })

      rootElement.selectAll('.matrix-cell-vBorder').each(function (vBorder) {
         d3.select(this)
            .attr('style', vBorderStyles(matrix)(vBorder))
      })
   }
}

function createElement_ (
   { val },
   { title, matrix },
   parent
) {
   return () => {
      const strokeWidth = 0.5
      const highlightStrokeWidth = 0.5
      const highlightStrokeColor = 'blue'
      const w = 30, h = 30

      const [width, height] = [w * matrix.j + highlightStrokeWidth, h * matrix.i + highlightStrokeWidth]
      const hMargin = w / 2
      const vMargin = h / 2

      const rootElement = parent
         .append('svg')

      rootElement
         .attr('width', width + hMargin)
         .attr('height', height + vMargin)

      rootElement
         .append('text')
         .text(title === "intermediate" ? " " : title)
         .attr('x', hMargin / 2)
         .attr('y', vMargin / 2)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'middle')
         .attr('text-anchor', 'left')

      // group for the whole matrix (rects and texts)
      const matrixGrp = rootElement
         .append('g')
         .attr('transform', `translate(${highlightStrokeWidth / 2 + hMargin / 2}, ${highlightStrokeWidth / 2 + vMargin})`)
         // these will be inherited by text elements
         .attr('fill', 'currentColor')
         .attr('stroke', 'currentColor')
         .attr('stroke-width', '.25') // otherwise setting stroke makes it bold

      // group for each row of cells
      const rowGrp = matrixGrp
         .selectAll('g')
         .data([...matrix.cells.entries()].map(([i, ns]) => { return { i: i + 1, ns } }))
         .enter()
         .append('g')

      const cells = rowGrp
         .selectAll('g')
         .data(({ i, ns }) => [...ns.entries()].map(([j, n]) => { return { i, j: j + 1, n } }))
         .enter()

      cells
         .append('rect')
         .attr('x', ({j}) => (j - 1) * w)
         .attr('y', ({i}) => (i - 1) * h)
         .attr('width', w)
         .attr('height', h)
         .attr('class', 'matrix-cell')
         .attr('stroke-width', strokeWidth)

      cells
         .append('text')
         .text(({n}) => val(n))
         .attr('x', ({j}) => (j - 0.5) * w)
         .attr('y', ({i}) => (i - 0.5) * h)
         .attr('class', 'matrix-cell-text')
         .attr('text-anchor', 'middle')
         .attr('dominant-baseline', 'middle')
         .attr('pointer-events', 'none')

      // group for all highlight borders
      const bordersGrp = rootElement
         .append('g')
         .attr('transform', `translate(${highlightStrokeWidth / 2 + hMargin / 2}, ${highlightStrokeWidth / 2 + vMargin})`)
         .attr('fill', 'currentColor')
         .attr('stroke', highlightStrokeColor)
         .attr('stroke-width', highlightStrokeWidth)

      const hBordersGrp = bordersGrp
         .append('g')

      // group for each row of horizontal borders
      const hBordersRowGrps = hBordersGrp
         .selectAll('g')
         .data(d3.range(matrix.i + 1))
         .enter()
         .append('g')

      hBordersRowGrps.each(function(d) {
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

      const vBordersGrp = bordersGrp
         .append('g')

      // group for each row of vertical borders
      const vBordersRowGrps = vBordersGrp
      .selectAll('g')
      .data(d3.range(1, matrix.i + 1), i => i)
      .enter()
      .append('g')

      vBordersRowGrps.each(function(d) {
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

      return rootElement
   }
}

export var setSelection = x1 => x2 => x3 => x4 => x5 => setSelection_(x1, x2, x3, x4, x5)
export var createElement = x1 => x2 => x3 => createElement_(x1, x2, x3)
