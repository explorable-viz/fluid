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

function max_y (linePlot) {
   return Math.max(...linePlot.data.map(point => point.y._1))
}

function min_x (linePlot) {
   return Math.min(...linePlot.data.map(point => point.x._1))
}

function max_x (linePlot) {
   return Math.max(...linePlot.data.map(point => point.x._1))
}

function drawLineChart_ (
   id,
   childIndex,
   {
      caption,   // String
      plots,     // Array LinePlot
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const margin = {top: 15, right: 65, bottom: 40, left: 30},
            width = 230 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom,
            y_max = Math.max(...plots.map(max_y)),
            x_min = Math.min(...plots.map(min_x)),
            x_max = Math.max(...plots.map(max_x)),
            names = plots.map(plot => plot.name._1)
      const div = d3.select('#' + id)

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom)
         .attr('id', childId)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const x = d3.scaleLinear().domain([x_min, x_max]).range([0, width]),
            y = d3.scaleLinear().domain([0, y_max]).range([height, 0])

      const line1 = d3.line()
         .x(d => x(d.x._1))
         .y(d => y(d.y._1))

      const color = d3.scaleOrdinal(d3.schemePastel1)

      svg.selectAll('lines')
         .data([...plots.entries()])
         .enter()
         .append('g')
         .append('path')
         .attr('fill', 'none')
         .attr('stroke', ([, d]) => color(names.indexOf(d.name._1)))
         .attr('stroke-width', 1)
         .attr('class', 'line')
         .attr('d', ([_, d]) => line1(d.data))

      const smallRadius = 2
      for (const n_plot of plots.entries()) {
         const [i, plot] = n_plot,
               col = color(names.indexOf(plot.name._1))
         svg.selectAll('markers')
            .data([...plot.data.entries()].map(([j, ns]) => [[i, j], ns]))
            .enter()
            .append('g')
            .append('circle')
            .attr('r', ([, d]) => d.y._2 ? smallRadius * 2 : smallRadius)
            .attr('cx', ([, d]) => x(d.x._1))
            .attr('cy', ([, d]) => y(d.y._1))
            .attr('fill', col)
            .attr('stroke', ([, d]) => d.y._2 ? colorShade(col, -30) : col)
            .on('mousedown', (e, d) => {
               console.log(`mousedown ${d[0]}`)
               listener(e)
            })
      }

      svg.append('g')
         .attr('transform', `translate(0, ${height})`)
         .call(d3.axisBottom(x).ticks(x_max - x_min).tickFormat(d3.format('d')))

      svg.append('g')
         .call(d3.axisLeft(y).tickSizeOuter(0).ticks(3).tickFormat(d3.format('.1f'))) // lots of hard-coded constants

      const legendLineHeight = 15,
            legendStart = width + margin.left / 2

      svg.append('rect')
         .attr('transform', `translate(${legendStart}, ${legendLineHeight * (names.length - 1) + 2})`)
         .attr('x', 0)
         .attr('y', 0)
         .attr('stroke', 'lightgray')
         .attr('fill', 'none')
         .attr('height', legendLineHeight * names.length)
         .attr('width', margin.right - 16)

      const legend = svg.selectAll('legend')
         .data(names)
         .enter()
         .append('g')
         .attr('class', 'legend')
         .attr('transform', (d, i) =>
            `translate(${legendStart}, ${height / 2 - margin.top + i * legendLineHeight})`
         )

      legend.append('text')
         .text(d => d)
         .attr('font-size', 11)
         .attr('transform', 'translate(15, 9)') // align text with boxes

      legend.append('circle')
         .attr('fill', d => color(names.indexOf(d)))
         .attr('r', smallRadius)
         .attr('cx', legendLineHeight / 2 - smallRadius / 2)
         .attr('cy', legendLineHeight / 2 - smallRadius / 2)

      svg.append('text')
         .text(caption._1)
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawLineChart = curry4(drawLineChart_);
