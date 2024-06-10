"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   { bar_attrs },
   indexCol,
   rootElement,
   chart,
   listener
) {
   rootElement.selectAll('.bar').each(function (bar) {
      d3.select(this) // won't work inside arrow function :/
         .attrs(bar_attrs(indexCol)(chart)(bar))
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
}

function drawBarChart_ (
   barChartHelpers,
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         caption,
         stackedBars,
      }
   },
   listener
) {
   return () => {
      const { val } = uiHelpers
      const { tickEvery } = barChartHelpers
      const childId = divId + '-' + suffix
      const margin = {top: 15, right: 75, bottom: 40, left: 40},
            width = 275 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom
      const div = d3.select('#' + divId)
      const color = d3.scaleOrdinal(d3.schemeAccent)
      let rootElement = div.selectAll('#' + childId)

      if (rootElement.empty()) {
         rootElement = div
            .append('svg')
               .attr('width', width + margin.left + margin.right)
               .attr('height', height + margin.top + margin.bottom)
            .attr('id', childId)

         rootElement
            .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

         // x-axis
         const x = d3.scaleBand()
            .range([0, width])
            .domain(stackedBars.map(bar => val(bar.x)))
            .padding(0.2)

         rootElement.append('g')
            .attr('transform', "translate(0," + height + ")")
            .call(d3.axisBottom(x))
            .selectAll('text')
               .style('text-anchor', 'middle')

         function barHeight (bars) {
            return bars.reduce((acc, bar) => { return val(bar.z) + acc }, 0)
         }
         // y-axis
         const nearest = 100,
               y_max = Math.ceil(Math.max(...stackedBars.map(d => barHeight(d.bars))) / nearest) * nearest
         const y = d3.scaleLinear()
            .domain([0, y_max])
            .range([height, 0])
         const tickEvery_n = tickEvery(y_max),
               ticks = Array.from(Array(Math.ceil(y_max / tickEvery_n + 1)).keys()).map(n => n * tickEvery_n)
         const yAxis = d3.axisLeft(y)
            .tickValues(ticks)

         rootElement.append('g')
            .call(yAxis)

         // bars
         const stacks = rootElement.selectAll('.stack')
            .data([...stackedBars.entries()])
            .enter()
            .append('g')
         const strokeWidth = 1

         stacks.selectAll('.bar')
            .data(([i, {x, bars}]) => bars.slice(1).reduce((acc, bar) => {
               const prev = acc[acc.length - 1]
               const y = prev.y + prev.height
               acc.push({i, j: prev.j + 1, x: val(x), y, height: val(bar.z)})
               return acc
            }, [{i, j: 0, x: val(x), y: 0, height: val(bars[0].z)}]))
            .enter()
            .append('rect')
               .attr('class', 'bar')
               .attr('x', bar => { return x(bar.x) })
               .attr('y', bar => { return y(bar.y + bar.height) })
               .attr('width', x.bandwidth())
               .attr('height', bar => { return height - y(bar.height) - strokeWidth }) // stop bars overplotting
               .attr('stroke-width', _ => strokeWidth)

         // TODO: enforce that all stacked bars have same set of segments
         const legendLineHeight = 15,
               legendStart = width + margin.left / 2
               names = stackedBars[0].bars.map(bar => val(bar.y))
         rootElement
            .append('rect')
            .attr('transform', `translate(${legendStart}, ${height / 2 - margin.top - 2})`)
            .attr('x', 0)
            .attr('y', 0)
            .attr('stroke', 'lightgray')
            .attr('fill', 'none')
            .attr('height', legendLineHeight * names.length)
            .attr('width', margin.right - 22)

         const legend = rootElement.selectAll('legend')
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

         rootElement
            .append('text')
            .text(val(caption))
            .attr('x', width / 2)
            .attr('y', height + 35)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'bottom')
            .attr('text-anchor', 'middle')
      }

      setSelState(barChartHelpers, color, rootElement, { stackedBars }, listener)
   }
}

export var drawBarChart = x1 => x2 => x3 => drawBarChart_(x1, x2, x3)
