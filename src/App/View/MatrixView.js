"use strict"

import * as d3 from "d3"

function intMatrix_nss (v) {
   return v._1
}

function intMatrix_i_max (v) {
   return v._2._1
}

function intMatrix_j_max (v) {
   return v._2._2
}

function drawMatrix_ (
   {
      uiHelpers: { val, selState, isPrimary𝕊, isSecondary𝕊 },
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
      const childId = divId + '-' + suffix
      const strokeWidth = 0.5
      const w = 30, h = 30
      const div = d3.select('#' + divId)
      const [width, height] = [w * intMatrix_j_max(matrix) + strokeWidth, h * intMatrix_i_max(matrix) + strokeWidth]
      const hMargin = w / 2
      const vMargin = h / 2

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
         .attr('id', childId)
         .attr('width', width + hMargin)
         .attr('height', height + vMargin)

      // group for each row
      const grp = svg
         .selectAll('g')
         .data([...intMatrix_nss(matrix).entries()].map(([i, ns]) => [i + 1, ns]))
         .enter()
         .append('g')
         .attr(
            'transform',
            (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`
         )

      const rect = grp
         .selectAll('rect')
         .data(([i, ns]) => [...ns.entries()].map(([j, n]) => [[i, j + 1], n]))
         .enter()

      rect
         .append('rect')
         .attr('x', (_, j) => w * j)
         .attr('width', w)
         .attr('height', h)
         .attr('class', ([, n]) =>
            isPrimary𝕊(selState(n).persistent)
            ? 'matrix-cell-selected'
            : isSecondary𝕊(selState(n).persistent)
               ? 'matrix-cell-selected-secondary'
               : 'matrix-cell-unselected')
         .attr('stroke-width', strokeWidth)

      rect
         .append('text')
         .text(([, n]) => val(n))
         .attr('x', (_, j) => w * (j + 0.5))
         .attr('y', 0.5 * h)
         .attr('class', 'matrix-cell-text')
         .attr('text-anchor', 'middle')
         .attr('dominant-baseline', 'middle')
         .attr('pointer-events', 'none')

      svg.append('text')
         .text(title)
         .attr('x', hMargin / 2)
         .attr('y', vMargin / 2)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'middle')
         .attr('text-anchor', 'left')

      svg.selectAll('rect')
         .on('mousedown', e => { listener(e) })
   }
}

export var drawMatrix = x1 => x2 => drawMatrix_(x1, x2)
