"use strict"

import * as d3 from "d3";

// This prelude currently duplicated across all FFI implementations.
function curry2(f) {
   return x1 => x2 => f(x1, x2)
}

function curry3(f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4(f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
}

// https://stackoverflow.com/questions/5560248
function colorShade(col, amt) {
   col = col.replace(/^#/, '')
   if (col.length === 3) col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2]

   let [r, g, b] = col.match(/.{2}/g);
   ([r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt])

   r = Math.max(Math.min(255, r), 0).toString(16)
   g = Math.max(Math.min(255, g), 0).toString(16)
   b = Math.max(Math.min(255, b), 0).toString(16)

   const rr = (r.length < 2 ? '0' : '') + r
   const gg = (g.length < 2 ? '0' : '') + g
   const bb = (b.length < 2 ? '0' : '') + b

   return `#${rr}${gg}${bb}`
}

function drawMatrix (
   id,
   childIndex,
   {
      title,                                                               // String
      matrix: { value0: { value0: nss, value1: i_max }, value1: j_max }    // IntMatrix
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const strokeWidth = 0.5
      const w = 30, h = 30
      const div = d3.select('#' + id)
      const [width, height] = [w * j_max + strokeWidth, h * i_max + strokeWidth]
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
         .data([...nss.entries()].map(([i, ns]) => [i + 1, ns]))
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
         .attr('class', ([, n]) => n.value1 ? 'matrix-cell-selected' : 'matrix-cell-unselected')
         .attr('stroke-width', strokeWidth)

      rect
         .append('text')
         .text(([, n]) => n.value0)
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
         .on('mousedown', (e, d) => {
            console.log(`mousedown ${d[0]}`)
            listener(e)
         })
   }
}

export var drawMatrix = curry4(drawMatrix);
