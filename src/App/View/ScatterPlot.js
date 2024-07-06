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

function Sel_isNone (v) {
   return v.tag == "None"
}

function Sel_isPrimary (v) {
   return v.tag == "Primary"
}

function Sel_isSecondary (v) {
   return v.tag == "Secondary"
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

function drawScatterPlot_ (
   id,
   suffix,
   {
      caption, // String
      data,    // Array ScatterRecord
      xlabel,
      ylabel
   },
   listener
) {
   return () => {
      const childId = id + '-' + suffix
      var max_width = 360
      var max_height = 360
      const x_max = Math.ceil(Math.max(...data.map(d => fst(d.x))))
      const x_min = Math.ceil(Math.min(...data.map(d => fst(d.x))))
      const y_max = Math.ceil(Math.max(...data.map(d => fst(d.y))))
      const y_min = Math.ceil(Math.min(...data.map(d => fst(d.y))))

      const margin = {top: 20, right: 20, bottom: 40, left: 50}

      const width = max_width - margin.left - margin.right,
            height = max_height - margin.top - margin.bottom
      const div = d3.select('#' + id)

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
            .attr('width', max_width + margin.left + margin.right)
            .attr('height', max_height + margin.top)
         .attr('id', childId)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const x = d3.scaleLinear()
         .domain([Math.min(0, x_min), x_max])
         .range([0, width])
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain([Math.min(0, y_min), y_max])
         .range([height, 0])
      svg.append('g')
         .call(d3.axisLeft(y))

      svg.append("text")
         .attr("x", width)
         .attr("y", height + 25)
         .style("text-anchor", "end")
         .style("font-size", "8px")
         .text(fst(xlabel))
      svg.append("text")
         .attr("transform", "rotate(-90)")
         .attr("x", -margin.top)
         .attr("y", -margin.left + 20)
         .style("text-anchor", "end")
         .style("font-size", "8px")
         .text(fst(ylabel))

         svg.append('g')
            .selectAll('dot')
            .data([...data.entries()])
            .enter()
            .append('circle')
               .attr('cx', ([, d]) => x(fst(d.x)))
               .attr('cy', ([, d]) => y(fst(d.y)))
               .attr('r', 3)
               .attr('data-y', ([, d]) => fst(d.y))
               .attr('data-x', ([, d]) => fst(d.x))
               .attr('stroke-width', 0.5)
               .attr('class', ([, d]) => {
                  console.log(fst(d.x))
                  console.log(Sel_isNone(snd(d.x)))
                  console.log(fst(d.y))
                  console.log(Sel_isNone(snd(d.y)))
                  return Sel_isNone(snd(d.x)) && Sel_isNone(snd(d.y)) ? 'scatterplot-point-unselected' : 'scatterplot-point-selected'
   }           )
               .on('mousedown', (e, d) => {listener(e)})

         svg.append('text')
            .text(fst(caption))
            .attr('x', width/2)
            .attr('y', height+40)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'bottom')
            .attr('text-anchor', 'middle')
   }
}

export var drawScatterPlot = curry4(drawScatterPlot_)