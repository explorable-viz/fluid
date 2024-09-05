"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

d3.selection.prototype.attrFuns = function(m) {
   for (const k in m) {
      this.attr(k, d => m[k](d))
   }
   return this
}

function createChild_ (parent, elementType, attrs) {
   return () => {
      return parent.append(elementType).attrs(attrs)
   }
}

function createChildren_ (parent, elementType, data, attrFuns) {
   return () => {
      return parent
         .selectAll(elementType)
         .data(data)
         .enter()
         .append(elementType)
         .attrFuns(attrFuns)
   }
}

function line_ (to, points) {
   return () => {
      const line = d3.line()
         .x(d => to.x(d.x))
         .y(d => to.y(d.y))
      return line(points)
   }
}

function xAxis_ (to, ticks) {
   return d3.axisBottom(to.x).ticks(ticks.x).tickFormat(d3.format('d'))
}

function yAxis_ (to, ticks) {
   return d3.axisLeft(to.y).tickSizeOuter(0).ticks(ticks.y).tickFormat(d3.format('.1f'))
}

function setSelState (
   { point_attrs },
   { },
   nameCol,
   rootElement,
   listener
) {
   rootElement.selectAll('.linechart-point').each(function (point) {
      d3.select(this) // won't work inside arrow function :/
         .attrs(point_attrs(nameCol)(point))
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
}

function drawLineChart_ (
   lineChartHelpers,
   uiHelpers,
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
      const { createRootElement, interior, ticks, to, legendHelpers, createAxes, createLegend, createLegendEntry, caption_attrs }
         = lineChartHelpers
      const { val } = uiHelpers
      const childId = divId + '-' + suffix
      const names = plots.map(plot => val(plot.name))
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
         rootElement = createRootElement(div)(childId)()

         rootElement.selectAll('line')
            .data([...plots.entries()])
            .enter()
            .append('path')
            .attr('fill', 'none')
            .attr('stroke', ([, plot]) => nameCol(val(plot.name)))
            .attr('stroke-width', 1)
            .attr('class', 'line')
            .attr('d', ([, plot]) => line(to)(plot.points.map(({ x, y }) => { return { x: val(x), y: val(y) } }))())

         for (const [i, plot] of plots.entries()) {
            rootElement.selectAll('linechart-point')
               .data([...plot.points.entries()].map(([j, { x, y } ]) => {
                  return { name: val(plot.name), x: val(x), y: val(y), i, j }
               }))
               .enter()
               .append('circle')
               .attr('class', 'linechart-point')
         }

         createAxes()
         const xAxis_g = rootElement
            .append('g')
            .attr('class', 'x-axis')
            .attr('transform', `translate(0, ${interior.height})`)
         xAxis_(to, ticks)(xAxis_g)

         const yAxis_g = rootElement
            .append('g')
            .attr('class', 'y-axis')
         yAxis_(to, ticks)(yAxis_g)

         const legend = createLegend(rootElement)()
         const legendEntry = createLegendEntry(legend)()

         legendEntry.append('text')
            .text(({ name }) => name)
            .attrs(legendHelpers.text_attrs)

         legendEntry.append('circle')
            .attr('fill', ({ name }) => nameCol(name))
            .attrs(legendHelpers.circle_attrs)

         rootElement
            .append('text')
            .text(val(caption))
            .attrs(caption_attrs)
      }
      setSelState(lineChartHelpers, uiHelpers, nameCol, rootElement, listener)
   }
}

export var drawLineChart = x1 => x2 => x3 => x4 => drawLineChart_(x1, x2, x3, x4)
export var scaleLinear = x1 => x2 => d3.scaleLinear().domain([x1.min, x1.max]).range([x2.min, x2.max])
export var createChild = x1 => x2 => x3 => createChild_(x1, x2, x3)
export var createChildren = x1 => x2 => x3 => x4 => createChildren_(x1, x2, x3, x4)
export var line = x1 => x2 => line_(x1, x2)
export var xAxis = x1 => x2 => xAxis_(x1, x2)
export var yAxis = x1 => x2 => yAxis_(x1, x2)
