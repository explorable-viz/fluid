"use strict"

import * as d3 from "d3"

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

// Heuristic saying how often to place a tick on an axis of length n.
function tickEvery (n) {
   const m = Math.floor(Math.log10(n))
   return n <= 2 * 10 ** m
      ? 2 * 10 ** (m - 1)
      : 10 ** m
}

function drawBarChart_ (
   id,
   suffix,
   {
      caption,    // String
      data,       // Array StackedBar
   },
   listener
) {
   return () => {
      const childId = id + '-' + suffix
      const margin = {top: 15, right: 75, bottom: 40, left: 40},
            width = 275 - margin.left - margin.right,
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

      // x-axis
      const x = d3.scaleBand()
         .range([0, width])
         .domain(data.map(d => fst(d.x)))
         .padding(0.2)
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
            .style('text-anchor', 'middle')

      function barHeight (bars) {
         return bars.reduce((acc, bar) => { return fst(bar.z) + acc }, 0)
      }
      // y-axis
      const nearest = 100,
            y_max = Math.ceil(Math.max(...data.map(d => barHeight(d.bars))) / nearest) * nearest
      const y = d3.scaleLinear()
         .domain([0, y_max])
         .range([height, 0])
      const tickEvery_n = tickEvery(y_max),
            ticks = Array.from(Array(Math.ceil(y_max / tickEvery_n + 1)).keys()).map(n => n * tickEvery_n)
      const yAxis = d3.axisLeft(y)
         .tickValues(ticks)
      svg.append('g')
         .call(yAxis)

      // bars
      const stacks = svg.selectAll('.stack')
         .data([...data.entries()])
         .enter()
         .append('g')
      const color = d3.scaleOrdinal(d3.schemeAccent)
      const strokeWidth = 1

      stacks.selectAll('.bar')
         .data(([i, {x, bars}]) => bars.slice(1).reduce((acc, bar) => {
            const prev = acc[acc.length - 1]
            const y = prev.y + prev.height
            acc.push({i, j: prev.j + 1, x: fst(x), y, height: fst(bar.z), sel: snd(bar.z)})
            return acc
         }, [{i, j: 0, x: fst(x), y: 0, height: fst(bars[0].z), sel: snd(bars[0].z)}]))
         .enter()
         .append('rect')
            .attr('x', bar => { return x(bar.x) })
            .attr('y', bar => { return y(bar.y + bar.height) })
            .attr('width', x.bandwidth())
            .attr('height', bar => { return height - y(bar.height) - strokeWidth }) // stop bars overplotting
            .attr('fill', bar => {
               const col = color(bar.j)
               return Sel_isNone(bar.sel) ? col : colorShade(col, -20)
            })
            .attr('stroke-width', _ => strokeWidth)
            .attr('stroke', bar => {
               const col = color(bar.j)
               return Sel_isNone(bar.sel) ? col : colorShade(col, -70)
            })
            .on('mousedown', (e, d) => { listener(e) })

      // TODO: enforce that all stacked bars have same set of segments
      const legendLineHeight = 15,
            legendStart = width + margin.left / 2
            names = data[0].bars.map(bar => fst(bar.y))
      svg.append('rect')
         .attr('transform', `translate(${legendStart}, ${height / 2 - margin.top - 2})`)
         .attr('x', 0)
         .attr('y', 0)
         .attr('stroke', 'lightgray')
         .attr('fill', 'none')
         .attr('height', legendLineHeight * names.length)
         .attr('width', margin.right - 22)

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

      const legendSquareSize = 4

      legend.append('rect')
         .attr('fill', d => color(names.indexOf(d)))
         .attr('width', legendSquareSize)
         .attr('height', legendSquareSize)
         .attr('x', legendLineHeight / 2 - legendSquareSize / 2)
         .attr('y', legendLineHeight / 2 - legendSquareSize)

      svg.append('text')
         .text(fst(caption))
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawBarChart = curry4(drawBarChart_)
