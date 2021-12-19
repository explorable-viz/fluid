"use strict"

const d3 = require("d3")
const shared = require("/src/app/Shared")

function drawMatrix (
   id,
   {
      title,                                                               // String
      matrix: { value0: { value0: nss, value1: i_max }, value1: j_max }    // IntMatrix
   },
   listener
) {
   return () => {
      const strokeWidth = 0.5
      const w = 30, h = 30
      const div = d3.select('#' + id)
      const [width, height] = [w * j_max + strokeWidth, h * i_max + strokeWidth]
      const hMargin = w / 2
      const vMargin = h / 2

      div.selectAll('*').remove()

      const svg = div
         .append('svg')
         .attr('width', width + hMargin)
         .attr('height', height + vMargin)

      // group for each row
      const grp = svg
         .selectAll('g')
         .data([...nss.entries()])
         .enter()
         .append('g')
         .attr(
            'transform', 
            (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`
         )

      const rect = grp
         .selectAll('rect')
         .data(([i, ns]) => [...ns.entries()].map(([j, n]) => [[i, j], n]))
         .enter()

      rect
         .append('rect')
         .attr('x', (_, j) => w * j)
         .attr('width', w)
         .attr('height', h)
         .attr('class', ([, n]) => n.value1 ? 'matrix-cell-selected' : 'matrix-cell-unselected')
         .attr('stroke-width', strokeWidth)
         .on('mouseover', (e, d) =>
            listener(e)
         )

      rect
         .append('text')
         .text(([, n]) => n.value0)
         .attr('x', (_, j) => w * (j + 0.5))
         .attr('y', 0.5 * h)
         .attr('class', 'matrix-cell-text')
         .attr('text-anchor', 'middle')
         .attr('dominant-baseline', 'middle')

      svg.append('text')
         .text(title)
         .attr('x', hMargin / 2)
         .attr('y', vMargin / 2)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'middle')
         .attr('text-anchor', 'left')
   }
}

exports.drawMatrix = shared.curry3(drawMatrix)
