"use strict"

const d3 = require("d3")

function drawMatrix (i, j) {
   return () => {
      const div = d3.select('#app-root'),
            svg = div.append('svg')
                        .attr('width', 200)
                        .attr('height', 200)
                        .attr('fill', 'green')
      const w = 30, h = 30, gap = 1.15
      const data = Array(i).fill(d3.range(j))
      const grp = svg.selectAll('g')
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
}

function curry2 (f) {
   return x => y => f(x, y)
}

exports.drawMatrix = curry2(drawMatrix)
