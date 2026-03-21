"use strict"

import * as d3 from "d3"


function setSelection_ (
   { point_attrs, eventListener, withScatterPlotPoint },
   {
      selState,
      selClasses,
      selClassesFor,
      join
   },
   view,
   select,
   rootElement
) {
   return () => {
      var newListener = eventListener(withScatterPlotPoint(select))()
      const { points } = view
      rootElement.selectAll('.scatterplot-point').each(function (point) {
         const sel = join(selState(points[point.i].x))(selState(points[point.i].y))
         d3.select(this) // won't work inside arrow function :/
            .classed(selClasses, false)
            .classed(selClassesFor(sel), true)
            .attrs(point_attrs(view)(point))
            .on('mousedown', e => { newListener(e) })
            .on('mouseenter', e => { newListener(e) })
            .on('mouseleave', e => { newListener(e) })
      })
   }
}

function createElement_ (
   { val },
   { caption, points, labels },
   parent
) {
   return () => {
      var max_width = 280
      var max_height = 200
      const x_max = Math.ceil(Math.max(...points.map(point => val(point.x))))
      const x_min = Math.ceil(Math.min(...points.map(point => val(point.x))))
      const y_max = Math.ceil(Math.max(...points.map(point => val(point.y))))
      const y_min = Math.ceil(Math.min(...points.map(point => val(point.y))))
      const margin = {top: 20, right: 20, bottom: 40, left: 50}
      const width = max_width - margin.left - margin.right,
            height = max_height - margin.top - margin.bottom

      const rootElement = parent
         .append('svg')
            .classed('center', true)
            .attr('width', max_width + margin.left + margin.right)
            .attr('height', max_height + margin.top)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const x = d3.scaleLinear()
         .domain([Math.min(0, x_min), x_max])
         .range([0, width])
      rootElement.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x).tickSizeOuter(0))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain([Math.min(0, y_min), y_max])
         .range([height, 0])
      rootElement.append('g')
         .call(d3.axisLeft(y).tickSizeOuter(0))

      rootElement.append("text")
         .attr("x", width)
         .attr("y", height + 25)
         .style("text-anchor", "end")
         .style("font-size", "10px")
         .text(val(labels.x))
      rootElement.append("text")
         .attr("transform", "rotate(-90)")
         .attr("x", -margin.top)
         .attr("y", -margin.left + 20)
         .style("text-anchor", "end")
         .style("font-size", "10px")
         .text(val(labels.y))

      rootElement.append('g')
         .selectAll('circle')
         .data([...points.entries()].map(([i, point]) => { return { i, point } }))
         .enter()
         .append('circle')
         .classed('scatterplot-point', true)
         .attr('cx', ({ point }) => x(val(point.x)))
         .attr('cy', ({ point }) => y(val(point.y)))
         .attr('stroke-width', 0.5)

      rootElement.append('text')
         .text(val(caption))
         .attr('x', width / 2)
         .attr('y', height + 40)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')

      return rootElement
   }
}

export var createElement = x1 => x2 => x3 => createElement_(x1, x2, x3)
export var setSelection = x1 => x2 => x3 => x4 => x5 => setSelection_(x1, x2, x3, x4, x5)
