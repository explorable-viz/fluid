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

function fst(p) {
   return p._1
}

function snd(p) {
   return p._2
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
      var max_width = 320
      var max_height = 185
      const x_max = Math.ceil(Math.max(...data.map(d => fst(d.x))))
      const y_max = Math.ceil(Math.max(...data.map(d => fst(d.y))))
      const z_max = Math.ceil(Math.max(...data.map(d => fst(d.z))))

      const childId = id + '-' + childIndex
      const margin = {top: 15, right: 0, bottom: 40, left: 40}
      // if (x_max + z_max + margin.left + margin.right > max_width) {
      //    max_width = x_max +  z_max  + margin.left + margin.right
      // }
      // if (y_max + z_max + margin.top + margin.bottom > max_height) {
      //    max_height = y_max + z_max + margin.top + margin.bottom
      // }

      const width = max_width - margin.left - margin.right,
            height = max_height - margin.top - margin.bottom
      const div = d3.select('#' + id)

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
            .attr('width', max_width + margin.left + margin.right)
            .attr('height', max_height + margin.top + margin.bottom)
         .attr('id', childId)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const x = d3.scaleLinear()
         .domain([0,x_z_max + margin.right])
         .range([0, width])
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain([0, y_max + z_max + margin.top])
         .range([height,0])
      svg.append('g')
         .call(d3.axisLeft(y))

      const z = d3.scaleLinear()
         .domain([1, z_max])
         .range([1, 30])

      unique_countries = data.map(d => fst(d.c))
      console.log(unique_countries)
      const c = d3.scaleOrdinal()
         .domain(unique_countries)
         .range(d3.schemeSet1)

      svg.append('g')
         .selectAll('dot')
         .data([...data.entries()])
         .enter()
         .append('circle')
            .attr('cx', ([, d]) => x(fst(d.x)))
            .attr('cy', ([, d]) => y(fst(d.y)))
            .attr('r', ([, d]) => z(fst(d.z)))
            .attr('stroke', 'black')
            .style('fill', ([, d]) => snd(d.x) || snd(d.y) || snd(d.z) ? colorShade(c(fst(d.c)), -40) : c(fst(d.c)))
            .style('class', ([, d]) => snd(d.x) || snd(d.y) || snd(d.z) ? 'dot-selected' : 'dot-unselected')
            .on('mousedown', (e, d) => { listener(e) })

      svg.selectAll("mylabels")
         .data(unique_countries)
         .enter()
         .append("text")
         .attr("x", max_width - 40)
         .attr("y", (d, i) => i * 20 + 10)
         .style("fill", d => c(d))
         .text(d => d)
         .attr("text-anchor", "left")
         .attr("alignment-baseline", "middle")

      svg.append('text')
         .text(fst(caption))
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawBubbleChart = curry4(drawBubbleChart_)
