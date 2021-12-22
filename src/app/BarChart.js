"use strict"

const d3 = require("d3")
const d3tip = require("d3-tip")
const shared = require("/src/app/Shared")

function drawBarChart (
   id,
   childIndex,
   {
      caption,    // String
      data_,        // Array BarChartRecord
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const margin = {top: 15, right: 0, bottom: 40, left: 30},
            width = 200 - margin.left - margin.right,
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

      const tip = d3tip.default()
         .attr('class', 'd3-tip')
         .offset([0, 0])
         .html((_, d) => d.y.value0)

      svg.call(tip)

      // x-axis
      const x = d3.scaleBand()
         .range([0, width])
         .domain(data_.map(d => d.x.value0))
         .padding(0.2)
      svg.append('g')
         .attr('transform', "translate(0," + height + ")")
         .call(d3.axisBottom(x))
         .selectAll('text')
            .style('text-anchor', 'middle')

      // y-axis
      const nearest = 100,
            y_max = Math.ceil(Math.max(...data_.map(d => d.y.value0)) / nearest) * nearest
      const y = d3.scaleLinear()
         .domain([0, y_max])
         .range([height, 0])
      const tickEvery = nearest / 2,
            ticks = Array.from(Array(y_max / tickEvery + 1).keys()).map(n => n * tickEvery)
      const yAxis = d3.axisLeft(y)
         .tickValues(ticks)
      svg.append('g')
         .call(yAxis)

      // bars
      const barFill = '#dcdcdc'
      svg.selectAll('rect')
         .data([...data_.entries()].map(([i, ns]) => [i + 1, ns]))
         .enter()
         .append('rect')
            .attr('x', ([, d]) => x(d.x.value0))
            .attr('y', ([, d]) => y(d.y.value0 + 1))  // ouch: bars overplot x-axis!
            .attr('width', x.bandwidth())
            .attr('height', ([, d]) => height - y(d.y.value0))
            .attr('fill', ([, d]) => d.y.value1 ? shared.colorShade(barFill, -40) : barFill)
            .attr('stroke', ([, d]) => d.y.value1 ? 'coral' : '')
            .on('mousedown', (e, d) => {
               console.log(`mousedown ${d[0]}`)
               listener(e)
            })

      svg.append('text')
         .text(caption.value0)
         .attr('x', width / 2)
         .attr('y', height + 35)
         .attr('class', 'title-text')
         .attr('dominant-baseline', 'bottom')
         .attr('text-anchor', 'middle')
   }
}

exports.drawBarChart = shared.curry4(drawBarChart)
