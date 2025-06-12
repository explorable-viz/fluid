"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

// CSS background-image + gradient fill doesn't work with SVG
// SVG patterns don't support per-usage styling so generate one per colour
// Masks eliminate dependency on specific colours but seem to clip the stroke a bit
function addHatchPattern (rootElement, j, col_j) {
   let pattern = rootElement.append('pattern')
      .attr('id', 'diagonalHatch-' + j)
      .attr('patternUnits', 'userSpaceOnUse')
      .attr('width', 2)
      .attr('height', 2)
      .attr('patternTransform', 'rotate(45)')

   pattern.append('rect')
      .attr('width', 3.5)
      .attr('height', 3.5)
      .attr('fill', col_j)

   pattern.append('line')
      .attr('x1', 0)
      .attr('y', 0)
      .attr('x2', 0)
      .attr('y2', 3.5)
      .attr('stroke', 'rgba(255, 255, 255, 1)')
      .attr('stroke-width', "1")
}

const color = d3.scaleOrdinal(d3.schemeAccent)

function setSelStates2_ (
   { bar_attrs, withBarChartSegment },
   view,
   select,
   rootElement
) {
   var newListener = withBarChartSegment(select)()
   return () => {
      rootElement.selectAll('.bar').each(function (bar) {
         d3.select(this) // won't work inside arrow function :/
            .attrs(bar_attrs(color)(view)(bar))
            .on('mousedown', e => { newListener(e) } )
            .on('mouseenter', e => { newListener(e) })
            .on('mouseleave', e => { newListener(e) })
      })
   }
}

function barAttrs (bar, strokeWidth, x, y, height) {
   bar
      .attr('class', 'bar')
      .attr('x', d => x(d.x))
      .attr('y', d => y(d.y + d.height))
      .attr('width', x.bandwidth())
      .attr('height', d => height - y(d.height) - strokeWidth)
      .attr('stroke-width', strokeWidth)
}

function barData(val, x) {
   return ([i, { x: xv, bars }]) =>
      bars.slice(1).reduce((acc, bar) => {
         const prev = acc[acc.length - 1]
         const y = prev.y + prev.height
         acc.push({ i, j: prev.j + 1, x: val(xv), y, height: val(bar.z) })
         return acc
      },
      [
         { i
         , j: 0
         , x: val(xv)
         , y: 0
         , height: val(bars[0].z) 
         }
      ]
   )
}

function createStacks (stackedBars, rootElement, strokeWidth, val, x, y, height) {
   const stacks = rootElement.selectAll('.stack')
      .data([...stackedBars.entries()])
      .enter()
      .append('g')
   
   stacks.selectAll('.bar')
      .data(barData(val, x))
      .enter()
      .append('rect')
      .call(bar => barAttrs(bar, strokeWidth, x, y, height))

   return stacks
}

function createRootElement2_ (
   barChartHelpers,
   uiHelpers,
   {
      caption,
      stackedBars
   },
   parent
) {
   return () => {
      const { val } = uiHelpers
      const { tickEvery } = barChartHelpers
      const margin = {top: 3, right: 75, bottom: 20, left: 40},
            width = 275 - margin.left - margin.right,
            height = 150 - margin.top - margin.bottom
      const rootElement = parent
         .append('svg')
            .attr('width', width + margin.left + margin.right)
            .attr('height', height + margin.top + margin.bottom + margin.bottom) // hackery from hell
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
            .attr('class', 'xaxis')

      function barHeight (bars) {
         return bars.reduce((acc, bar) => { return val(bar.z) + acc }, 0)
      }

      // y-axis
      const nearest = 10,
            y_max = Math.ceil(Math.max(...stackedBars.map(d => barHeight(d.bars))) / nearest) * nearest
      const y = d3.scaleLinear()
         .domain([0, y_max])
         .range([height, 0])
         .nice()
      const tickEvery_n = tickEvery(y_max),
            ticks = Array.from(Array(Math.ceil(y_max / tickEvery_n + 1)).keys()).map(n => n * tickEvery_n)
      const yAxis = d3.axisLeft(y)

      rootElement.append('g')
         .call(yAxis)

         
      const strokeWidth = 1
      // bars
      const stacks = createStacks(stackedBars, rootElement, strokeWidth, val, x, y, height) 
      // TODO: enforce that all stacked bars have same set of segments
      const j_max = Math.max(...stackedBars.map(bar => bar.bars.length))

      for (let j = 0; j < j_max; ++j) {
         addHatchPattern(rootElement, j, color(j))
      }

      return rootElement
   }
}

export var createRootElement2 = x1 => x2 => x3 => x4 => createRootElement2_(x1, x2, x3, x4)
export var setSelStates2 = x1 => x2 => x3 => x4 => setSelStates2_(x1, x2, x3, x4)
