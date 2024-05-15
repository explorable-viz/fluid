"use strict"

import * as d3 from "d3"

// =================================================================
// This prelude currently duplicated across all FFI implementations.
// =================================================================

function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

function curry3 (f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4 (f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
}

function isCtr (v, i, ctrs) {
   const j = ctrs.indexOf(v.tag)
   if (j == -1) {
      throw `Bad constructor ${v.tag}; expected one of ${ctrs}`
   }
   return i == j
}

// Selectable projections
function val(x) {
   return x._1
}

function selState(x) {
   return x._2
}

const 𝕊_ctrs = ["None", "Primary", "Secondary"]

function 𝕊_isNone (v) {
   return isCtr(v, 0, 𝕊_ctrs)
}

function 𝕊_isPrimary (v) {
   return isCtr(v, 1, 𝕊_ctrs)
}

function 𝕊_isSecondary (v) {
   return isCtr(v, 2, 𝕊_ctrs)
}

// https://stackoverflow.com/questions/5560248
function colorShade (col, amt) {
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

// =================================================================
// End of duplicated prelude
// =================================================================

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
   id,
   suffix,
   {
      title,    // String
      matrix    // IntMatrix
   },
   listener
) {
   return () => {
      const childId = id + '-' + suffix
      const strokeWidth = 0.5
      const w = 30, h = 30
      const div = d3.select('#' + id)
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
            Sel_isPrimary(selState(n).persistent)
            ? 'matrix-cell-selected'
            : Sel_isSecondary(SelState(n).persistent)
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

export var drawMatrix = curry4(drawMatrix_)
