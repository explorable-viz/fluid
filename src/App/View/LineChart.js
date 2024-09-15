"use strict"

import * as d3 from "d3"

function setSelState (
   { point_attrs },
   rootElement,
   listener
) {
   rootElement.selectAll('.linechart-point').each(function (point) {
      d3.select(this) // won't work inside arrow function :/
         .attrs(point_attrs(point))
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
      suffix
   },
   listener
) {
   return () => {
      const { createRootElement } = lineChartHelpers
      const childId = divId + '-' + suffix
      const div = d3.select('#' + divId)
      if (div.empty()) {
         console.error('Unable to insert figure: no div found with id ' + divId)
         return
      }

      let rootElement = div.selectAll('#' + childId)

      if (rootElement.empty()) {
         ({ rootElement } = createRootElement(div)(childId)())
      }
      setSelState(lineChartHelpers, rootElement, listener)
   }
}

export var drawLineChart = x1 => x2 => x3 => x4 => drawLineChart_(x1, x2, x3, x4)
