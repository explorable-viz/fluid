"use strict"

const d3 = require("d3")

function drawMatrix (el, i, j) {
   const w = 30, h = 30, gap = 1.15
   const data = Array(i).fill(d3.range(j))
   const grp = el.selectAll('g')
      .data(data)
      .enter()
      .append('g')
      .attr('transform', (d, i) => 'translate(0, ' + h * gap * i + ')')

   grp.selectAll('rect')
      .data(d => d)
      .enter()
      .append('rect')
         .attr('x', (d, j) => w * gap * j)
         .attr('width', w)
         .attr('height', h)
}

const div = d3.select('#app-root'),
      svg = div.append('svg')
            .attr('width', 200)
            .attr('height', 200)
            .attr('fill', 'green')
drawMatrix(svg, 5, 5)

function curry2 (f) {
   return x => y => f(x, y)
}

function curry3 (f) {
   return x => y => z => f(x, y, z)
}

exports.drawMatrix = curry3(drawMatrix)(svg)
