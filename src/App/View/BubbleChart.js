"use strict"

import * as d3 from "d3"

function drawBubbleChart_ (
   {
      uiHelpers: { val, selState, isNone𝕊, colorShade },
      divId,
      suffix,
      view: {
         caption, // String
         data,    // Array BubbleRecord
         xlabel,
         ylabel,
      }
   },
   listener
) {
   return () => {
      const childId = divId + '-' + suffix
      var max_width = 340
      var max_height = 190
      const max_z_rad = Math.min(max_width, max_height) / 10
      const x_max = Math.ceil(Math.max(...data.map(d => val(d.x))))
      const x_min = Math.floor(Math.min(...data.map(d => val(d.x))))
      const y_max = Math.ceil(Math.max(...data.map(d => val(d.y))))
      const y_min = Math.floor(Math.min(...data.map(d => val(d.y))))
      const z_max = Math.ceil(Math.max(...data.map(d => val(d.z))))
      const margin = {top: 20, right: 20, bottom: 40, left: 50}

      const width = max_width - margin.left - margin.right,
            height = max_height - margin.top - margin.bottom
      const div = d3.select('#' + divId)

      div.selectAll('#' + childId).remove()

      const svg = div
         .append('svg')
            .attr('width', max_width + margin.left + margin.right)
            .attr('height', max_height + margin.top + margin.bottom)
         .attr('id', childId)
         .append('g')
            .attr('transform', `translate(${margin.left}, ${margin.top})`)

      const x = d3.scaleLinear()
         .domain([Math.min(0, x_min),x_max])
         .range([0, width - max_z_rad])
      svg.append("text")
         .attr("transform", "rotate(-90)")
         .attr("x", -margin.top)
         .attr("y", -margin.left + 20)
         .style("text-anchor", "end")
         .style("font-size", "8px")
         .text(val(ylabel))

      svg.append("text")
         .attr("x", width)
         .attr("y", height + 25)
         .style("text-anchor", "end")
         .style("font-size", "8px")
         .text(val(xlabel))

      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
         .style('text-anchor', 'middle')

      const y = d3.scaleLinear()
         .domain([Math.min(0, y_min), y_max])
         .range([height, 0])
      svg.append('g')
         .call(d3.axisLeft(y))

      const z = d3.scaleLinear()
         .domain([1, z_max])
         .range([1, max_z_rad])

      unique_countries = [...new Set(data.map(d => val(d.c)))]
      console.log(data)
      console.log(unique_countries)
      const c = d3.scaleOrdinal()
         .domain(unique_countries)
         .range(d3.schemePastel1)

      svg.append('g')
         .selectAll('dot')
         .data([...data.entries()])
         .enter()
         .append('circle')
            .attr('cx', ([, d]) => x(val(d.x)))
            .attr('cy', ([, d]) => y(val(d.y)))
            .attr('r', ([, d]) => z(val(d.z)))
            .attr('stroke', ([, d]) =>
               isNone𝕊(selState(d.x).persistent) && isNone𝕊(selState(d.y).persistent) && isNone𝕊(selState(d.z).persistent)
               ? colorShade(c(val(d.c)))(-30) : 'black')
            .style('fill', ([, d]) =>
               isNone𝕊(selState(d.x).persistent) && isNone𝕊(selState(d.y).persistent) && isNone𝕊(selState(d.z).persistent)
               ? c(val(d.c)): colorShade(c(val(d.c)))(-50))
            .style('class', ([, d]) =>
               isNone𝕊(selState(d.x).persistent) && isNone𝕊(selState(d.y).persistent) && isNone𝕊(selState(d.z).persistent)
               ? 'dot-unselected' : 'dot-selected')
            .on('mousedown', (e, d) => { listener(e) })

      svg.selectAll("mylabels")
         .data(unique_countries)
         .enter()
         .append("text")
         .attr("x", max_width - 40)
         .attr("y", (d, i) => i * 20 + 10)
         .style("fill", d => c(d))
         .text(d => d)
         .attr("text-anchor", "left")
         .attr("alignment-baseline", "middle")

      svg.append('text')
         .text(val(caption))
         .attr('x', width / 2)
         .attr('y', height + 40)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

export var drawBubbleChart = x1 => x2 => drawBubbleChart_(x1, x2)
