"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   { point_attrs },
   nameCol,
   rootElement,
   chart,
   listener
) {
   rootElement.selectAll('.point').each(function (point) {
      d3.select(this) // won't work inside arrow function :/
         .attrs(point_attrs(nameCol)(chart)(point))
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
}

function drawLineChart_ (
   lineChartHelpers,
   { val },
   {
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
      const { point_smallRadius, plot_max_y, plot_max_x, plot_min_x } = lineChartHelpers
      const childId = divId + '-' + suffix
      const margin = {top: 15, right: 65, bottom: 40, left: 30},
            width = 230 - margin.left - margin.right,
            height = 185 - margin.top - margin.bottom,
            y_max = Math.max(...plots.map(plot_max_y)),
            x_min = Math.min(...plots.map(plot_min_x)),
            x_max = Math.max(...plots.map(plot_max_x)),
            names = plots.map(plot => val(plot.name))
      const div = d3.select('#' + divId)
      if (div.empty()) {
         console.error('Unable to insert figure: no div found with id ' + divId)
         return
      }

      let rootElement = div.selectAll('#' + childId)
      const color = d3.scaleOrdinal(d3.schemePastel1)

      function nameCol (name) {
         return color(names.indexOf(name))
      }

      if (rootElement.empty()) {
         rootElement = div
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

         rootElement.selectAll('line')
            .data([...plots.entries()])
            .enter()
            .append('g')
            .append('path')
            .attr('fill', 'none')
            .attr('stroke', ([, plot]) => nameCol(val(plot.name)))
            .attr('stroke-width', 1)
            .attr('class', 'line')
            .attr('d', ([, plot]) => line1(plot.points))

         for (const i_plot of plots.entries()) {
            const [i, plot] = i_plot
            rootElement.selectAll('point')
               .data([...plot.points.entries()].map(([j, p]) => {
                  return { name: val(plot.name), x: val(p.x), y: val(p.y), i, j }
               }))
               .enter()
               .append('g')
               .append('circle')
               .attr('class', 'point')
               .attr('cx', point => x(point.x))
               .attr('cy', point => y(point.y))
         }

         rootElement
            .append('g')
            .attr('transform', `translate(0, ${height})`)
            .call(d3.axisBottom(x).ticks(x_max - x_min).tickFormat(d3.format('d')))

         rootElement
            .append('g')
            .call(d3.axisLeft(y).tickSizeOuter(0).ticks(3).tickFormat(d3.format('.1f'))) // lots of hard-coded constants

         const legendLineHeight = 15,
               legendStart = width + margin.left / 2

         rootElement
            .append('rect')
            .attr('class', 'legend-box')
            .attr('transform', `translate(${legendStart}, ${legendLineHeight * (names.length - 1) + 2})`)
            .attr('x', 0)
            .attr('y', 0)
            .attr('height', legendLineHeight * names.length)
            .attr('width', margin.right - 16)

         const legend = rootElement.selectAll('legend')
            .data(names)
            .enter()
            .append('g')
            .attr('transform', (d, i) =>
               `translate(${legendStart}, ${height / 2 - margin.top + i * legendLineHeight})`
            )

         legend.append('text')
            .text(d => d)
            .attr('font-size', 11)
            .attr('transform', 'translate(15, 9)') // align text with boxes

         legend.append('circle')
            .attr('fill', d => nameCol(d))
            .attr('r', point_smallRadius)
            .attr('cx', legendLineHeight / 2 - point_smallRadius / 2)
            .attr('cy', legendLineHeight / 2 - point_smallRadius / 2)

         rootElement
            .append('text')
            .text(val(caption))
            .attr('x', width / 2)
            .attr('y', height + 35)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'bottom')
            .attr('text-anchor', 'middle')
      }
      setSelState(lineChartHelpers, nameCol, rootElement, { plots }, listener)
   }
}

export var drawLineChart = x1 => x2 => x3 => x4 => drawLineChart_(x1, x2, x3, x4)
export var drawLineChart2 = x1 => x2 => x3 => x4 => drawLineChart_(x1, x2, x3, x4)
