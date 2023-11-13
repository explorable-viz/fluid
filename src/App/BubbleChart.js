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

function drawBubbleChart_ (
   id,
   childIndex,
   {
      caption, // String
      data,   // Array BubbleRecord
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const margin = {top: 15, right: 0, bottom: 40, left: 40},
            width = 350 - margin.left - margin.right,
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
         .html((_, d) => d.y_1)

      svg.call(tip)

      const x_max = Math.ceil(Math.max(...data.map(d => d.x._1 + d.z._1)))
      const y_max = Math.ceil(Math.max(...data.map(d => d.y._1 + d.z._1)))
      const z_max = Math.ceil(Math.max(...data.map(d => d.z._1)))
      const x = d3.scaleLinear()
         .domain([0,x_max])
         .range([0, width])
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain([0, y_max])
         .range([height,0])
      svg.append('g')
         .call(d3.axisLeft(y))

      const z = d3.scaleLinear()
         .domain([1, z_max])
         .range([1, 30])

      const c = d3.scaleOrdinal()
         .domain([...new Set(...data.map(d => d.c._1))])
         .range(d3.schemeSet1)

      svg.append('g')
         .selectAll('dot')
         .data([...data.entries()])
         .enter()
         .append('circle')
            .attr('cx', ([, d]) => x(d.x._1))
            .attr('cy', ([, d]) => y(d.y._1))
            .attr('r', ([, d]) => z(d.z._1))
            .attr('stroke', 'black')
            .style('fill', ([, d]) => d.y._2 ? colorShade(c(d.c._1), -40) : c(d.c._1))
            .style('class', ([, d]) => d.y._2 ? 'dot-selected' : 'dot-unselected')
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

export var drawBubbleChart = curry4(drawBubbleChart_)