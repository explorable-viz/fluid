"use strict"

import * as d3 from "d3"

function setSelState (
   { point_attrs },
   rootElement,
   interior,
   listener
) {
   rootElement.selectAll('.linechart-point').each(function (point) {
      d3.select(this) // won't work inside arrow function :/
         .attrs(point_attrs(interior)(point))
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
         plots,     // Array LinePlot
      }
   },
   listener
) {
   return () => {
      const { createRootElement, legendHelpers, createLegend, createLegendEntry } = lineChartHelpers
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
         ({ rootElement, interior } = createRootElement(div)(childId)())
         const legend = createLegend(rootElement)()
         const legendEntry = createLegendEntry(legend)()

         legendEntry.append('text')
            .attr('class', 'legend-text')
            .text(({ name }) => name)
            .attrs(legendHelpers.text_attrs)

         legendEntry.append('circle')
            .attr('fill', ({ name }) => nameCol(name))
            .attrs(legendHelpers.circle_attrs)
      }
      setSelState(lineChartHelpers, rootElement, interior, listener)
   }
}

export var drawLineChart = x1 => x2 => x3 => x4 => drawLineChart_(x1, x2, x3, x4)
