"use strict"

import * as d3 from "d3"

// Heuristic saying how often to place a tick on an axis of length n.
function tickEvery (n) {
   const m = Math.floor(Math.log10(n))
   return n <= 2 * 10 ** m
      ? 2 * 10 ** (m - 1)
      : 10 ** m
}

function setSelectionState (stacks, selData) {
   stacks.selectAll('rect').each((d, i) => {
      console.log(selData[d.i])
   })
}

function drawBarChart_ (
   {
      uiHelpers: { val, selState, barChartHelpers: { bar_fill, bar_stroke } },
      divId,
      suffix,
      view: {
         chart: {
            caption,    // String
            data,       // Array StackedBar
         },
         selData        // BarChartSelState
      }
   },
   listener
) {
   return () => {
      const childId = divId + '-' + suffix
      const margin = {top: 15, right: 75, bottom: 40, left: 40},
            width = 275 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom
      const div = d3.select('#' + divId)

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
         .domain(data.map(d => val(d.x)))
         .padding(0.2)
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
            .style('text-anchor', 'middle')

      function barHeight (bars) {
         return bars.reduce((acc, bar) => { return val(bar.z) + acc }, 0)
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

      mouseenterEnabled = true
      stacks.selectAll('.bar')
         .data(([i, {x, bars}]) => bars.slice(1).reduce((acc, bar) => {
            const prev = acc[acc.length - 1]
            const y = prev.y + prev.height
            acc.push({i, j: prev.j + 1, x: val(x), y, height: val(bar.z), sel: selState(bar.z)})
            return acc
         }, [{i, j: 0, x: val(x), y: 0, height: val(bars[0].z), sel: selState(bars[0].z)}]))
         .enter()
         .append('rect')
            .attr('x', bar => { return x(bar.x) })
            .attr('y', bar => { return y(bar.y + bar.height) })
            .attr('width', x.bandwidth())
            .attr('height', bar => { return height - y(bar.height) - strokeWidth }) // stop bars overplotting
            .attr('fill', bar => { return bar_fill(bar.sel)(color(bar.j)) })
            .attr('stroke-width', _ => strokeWidth)
            .attr('stroke', bar => { return bar_stroke(bar.sel)(color(bar.j)) })
            .on('mousedown', (e, d) => { listener(e) })
            .on('mouseleave', (e, d) => {
               console.log(`${e.type}`)
               listener(e)
            })
            .on('mouseenter', (e, d) => {
               if (mouseenterEnabled) {
                  console.log(`${e.type}`)
                  listener(e)
               }
               mouseenterEnabled = !mouseenterEnabled
            })

      setSelectionState(stacks, selData)

      // TODO: enforce that all stacked bars have same set of segments
      const legendLineHeight = 15,
            legendStart = width + margin.left / 2
            names = data[0].bars.map(bar => val(bar.y))
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
         .text(val(caption))
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawBarChart = x1 => x2 => drawBarChart_(x1, x2)
