"use strict"

const d3 = require("d3")

function drawMatrix (nss) {
   return () => {
      const div = d3.select('#app-root'),
            svg = div.append('svg')
                     .attr('width', 200)
                     .attr('height', 200)
                     .attr('fill', 'grey')
      const w = 30, h = 30, gap = 1.15
      const grp = svg.selectAll('g')
         .data(nss)
         .enter()
         .append('g')
         .attr('transform', (_, i) => "translate(0, " + h * gap * i + ")")

      const rect = grp.selectAll('rect')
                      .data(d => d)
                      .enter()

      rect.append('rect')
          .attr('x', (_, j) => w * gap * j)
          .attr('width', w)
          .attr('height', h)

      rect.append('text')
          .attr('x', (_, j) => w * gap * j)
          .attr('fill', 'black')
          .text(d => `${d}`)
   }
}

function curry2 (f) {
   return x => y => f(x, y)
}

function curry3 (f) {
   return x => y => z => f(x, y, z)
}

exports.drawMatrix = drawMatrix
