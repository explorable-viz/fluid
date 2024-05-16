"use strict"

import * as d3 from "d3"
import * as d3tip from "d3-tip"

// =================================================================
// This prelude currently duplicated across all FFI implementations.
// =================================================================

function curry2 (f) {
   return x1 => x2 => f(x1, x2)
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

function drawScatterPlot_ (
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         caption, // String
         data,    // Array ScatterRecord
         xlabel,
         ylabel
      }
   },
   listener
) {
   return () => {
      const childId = divId + '-' + suffix
      var max_width = 360
      var max_height = 360
      const x_max = Math.ceil(Math.max(...data.map(d => val(d.x))))
      const x_min = Math.ceil(Math.min(...data.map(d => val(d.x))))
      const y_max = Math.ceil(Math.max(...data.map(d => val(d.y))))
      const y_min = Math.ceil(Math.min(...data.map(d => val(d.y))))

      const margin = {top: 20, right: 20, bottom: 40, left: 50}

      const width = max_width - margin.left - margin.right,
            height = max_height - margin.top - margin.bottom
      const div = d3.select('#' + divId)

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
         .text(val(xlabel))
      svg.append("text")
         .attr("transform", "rotate(-90)")
         .attr("x", -margin.top)
         .attr("y", -margin.left + 20)
         .style("text-anchor", "end")
         .style("font-size", "8px")
         .text(val(ylabel))


         svg.append('g')
            .selectAll('dot')
            .data([...data.entries()])
            .enter()
            .append('circle')
               .attr('cx', ([, d]) => x(val(d.x)))
               .attr('cy', ([, d]) => y(val(d.y)))
               .attr('r', 3)
               .attr('data-y', ([, d]) => val(d.y))
               .attr('stroke-width', 0.5)
               .attr('class', ([, d]) =>
                  𝕊_isNone(selState(d.x).persistent) && 𝕊_isNone(selState(d.y).persistent) ? 'scatterplot-point-unselected' : 'scatterplot-point-selected')
               .on('mousedown', (e, d) => {listener(e)})

         svg.append('text')
            .text(val(caption))
            .attr('x', width/2)
            .attr('y', height+40)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'bottom')
            .attr('text-anchor', 'middle')
   }
}

export var drawScatterPlot = curry2(drawScatterPlot_)
