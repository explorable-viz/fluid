"use strict"

import * as d3 from "d3"

function setSelState (
   {
      selState,
      selClasses,
      selClass,
      join
   },
   rootElement,
   { data },
   listener
) {
   rootElement.selectAll('.scatterplot-point').each(function (point) {
      const sel = join(selState(data[point.i].x))(selState(data[point.i].y))
      console.log("Setting " + selClass(sel) + " for " + point.i)
      d3.select(this) // won't work inside arrow function :/
         .classed(selClasses, false)
         .classed(selClass(sel), true)
         .on('mousedown', e => { listener(e) })
         .on('mouseenter', e => { listener(e) })
         .on('mouseleave', e => { listener(e) })
   })
}

function drawScatterPlot_ (
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         caption, // String
         data,    // Array Point
         xlabel,
         ylabel
      }
   },
   listener
) {
   return () => {
      const { val } = uiHelpers
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
      let rootElement = div.selectAll('#' + childId)

      if (rootElement.empty()) {
         rootElement = div
            .append('svg')
               .classed('center', true)
               .attr('width', max_width + margin.left + margin.right)
               .attr('height', max_height + margin.top)
            .attr('id', childId)
            .append('g')
               .attr('transform', `translate(${margin.left}, ${margin.top})`)

         const x = d3.scaleLinear()
            .domain([Math.min(0, x_min), x_max])
            .range([0, width])
         rootElement.append('g')
            .attr('transform', "translate(0," + height + ")")
            .call(d3.axisBottom(x))
            .selectAll('text')
            .style('text-anchor', 'middle')

         const y = d3.scaleLinear()
            .domain([Math.min(0, y_min), y_max])
            .range([height, 0])
         rootElement.append('g')
            .call(d3.axisLeft(y))

         rootElement.append("text")
            .attr("x", width)
            .attr("y", height + 25)
            .style("text-anchor", "end")
            .style("font-size", "8px")
            .text(val(xlabel))
         rootElement.append("text")
            .attr("transform", "rotate(-90)")
            .attr("x", -margin.top)
            .attr("y", -margin.left + 20)
            .style("text-anchor", "end")
            .style("font-size", "8px")
            .text(val(ylabel))

         rootElement.append('g')
            .selectAll('circle')
            .data([...data.entries()].map(([i, point]) => { return { i, point } }))
            .enter()
            .append('circle')
               .classed('scatterplot-point', true)
               .attr('cx', ({ point }) => x(val(point.x)))
               .attr('cy', ({ point }) => y(val(point.y)))
               .attr('r', 2)
//               .attr('data-y', ({ point }) => val(point.y))
               .attr('stroke-width', 0.5)

         rootElement.append('text')
            .text(val(caption))
            .attr('x', width / 2)
            .attr('y', height + 40)
            .attr('class', 'title-text')
            .attr('dominant-baseline', 'bottom')
            .attr('text-anchor', 'middle')
      }

      setSelState(uiHelpers, rootElement, { data }, listener)
   }
}

export var drawScatterPlot = x1 => x2 => drawScatterPlot_(x1, x2)
