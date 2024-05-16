"use strict"

import * as d3 from "d3"

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

function max_y ({ val }) {
   return linePlot => {
      return Math.max(...linePlot.data.map(point => val(point.y)))
   }
}

function min_x ({ val }) {
   return linePlot => {
      return Math.min(...linePlot.data.map(point => val(point.x)))
   }
}

function max_x ({ val }) {
   return linePlot => {
      return Math.max(...linePlot.data.map(point => val(point.x)))
   }
}

function drawLineChart_ (
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         caption,   // String
         plots,     // Array LinePlot
      }
   },
   listener
) {
   return () => {
      const { val, selState } = uiHelpers
      const childId = divId + '-' + suffix
      const margin = {top: 15, right: 65, bottom: 40, left: 30},
            width = 230 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom,
            y_max = Math.max(...plots.map(max_y(uiHelpers))),
            x_min = Math.min(...plots.map(min_x(uiHelpers))),
            x_max = Math.max(...plots.map(max_x(uiHelpers))),
            names = plots.map(plot => val(plot.name))
      const div = d3.select('#' + divId)

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
         .x(d => x(val(d.x)))
         .y(d => y(val(d.y)))

      const color = d3.scaleOrdinal(d3.schemePastel1)

      svg.selectAll('lines')
         .data([...plots.entries()])
         .enter()
         .append('g')
         .append('path')
         .attr('fill', 'none')
         .attr('stroke', ([, d]) => color(names.indexOf(val(d.name))))
         .attr('stroke-width', 1)
         .attr('class', 'line')
         .attr('d', ([_, d]) => line1(d.data))

      const smallRadius = 2
      for (const n_plot of plots.entries()) {
         const [i, plot] = n_plot,
               col = color(names.indexOf(val(plot.name)))
         svg.selectAll('markers')
            .data([...plot.data.entries()].map(([j, ns]) => [[i, j], ns]))
            .enter()
            .append('g')
            .append('circle')
            .attr('r', ([, d]) => 𝕊_isNone(selState(d.y).persistent) ? smallRadius : smallRadius * 2)
            .attr('cx', ([, d]) => x(val(d.x)))
            .attr('cy', ([, d]) => y(val(d.y)))
            .attr('fill', col)
            .attr('stroke', ([, d]) => 𝕊_isNone(selState(d.y).persistent) ? col : colorShade(col, -30))
            .on('mousedown', (e, d) => { listener(e) })
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
         .text(val(caption))
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawLineChart = curry2(drawLineChart_)
