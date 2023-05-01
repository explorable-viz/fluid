"use strict"

import * as d3 from "d3"
import * as d3tip from "d3-tip"

// This prelude currently duplicated across all FFI implementations.
function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

function curry3 (f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4 (f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
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

function drawBarChart_ (
   id,
   childIndex,
   {
      caption,    // String
      data,       // Array BarChartRecord
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const margin = {top: 15, right: 0, bottom: 40, left: 30},
            width = 200 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom
      const div = d3.select('#' + id)

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom)
         .attr('id', childId)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const tip = d3tip.default()
         .attr('class', 'd3-tip')
         .offset([0, 0])
         .html((_, d) => d.y._1)

      svg.call(tip)

      // x-axis
      const x = d3.scaleBand()
         .range([0, width])
         .domain(data.map(d => d.x._1))
         .padding(0.2)
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
            .style('text-anchor', 'middle')

      // y-axis
      const nearest = 100,
            y_max = Math.ceil(Math.max(...data.map(d => d.y._1)) / nearest) * nearest
      const y = d3.scaleLinear()
         .domain([0, y_max])
         .range([height, 0])
      const tickEvery = nearest / 2,
            ticks = Array.from(Array(y_max / tickEvery + 1).keys()).map(n => n * tickEvery)
      const yAxis = d3.axisLeft(y)
         .tickValues(ticks)
      svg.append('g')
         .call(yAxis)

      // bars
      const barFill = '#dcdcdc'
      svg.selectAll('rect')
         .data([...data.entries()])
         .enter()
         .append('rect')
            .attr('x', ([, d]) => x(d.x._1))
            .attr('y', ([, d]) => y(d.y._1 + 1))  // ouch: bars overplot x-axis!
            .attr('width', x.bandwidth())
            .attr('height', ([, d]) => height - y(d.y._1))
            .attr('fill', ([, d]) => d.y._2 ? colorShade(barFill, -40) : barFill)
            .attr('class', ([, d]) => d.y._2 ? 'bar-selected' : 'bar-unselected')
            .on('mousedown', (e, d) => {
               console.log(`mousedown ${d[0]}`)
               listener(e)
            })

      svg.append('text')
         .text(caption._1)
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawBarChart = curry4(drawBarChart_)
