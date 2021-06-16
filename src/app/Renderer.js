"use strict"

const d3 = require("d3")
const d3tip = require("d3-tip")

const cellFillDefault         = 'White',
      cellStroke              = 'DarkGray',
      cellTextFill            = 'Black',
      cellFontSize            = '10pt',
      fontFamily              = "Roboto, sans-serif",
      strokeWidth             = 0.5,
      titleTextFill           = 'Black',
      titleFontSize           = '9pt'

function drawMatrix (
   id, {
      title,                                                            // String
      matrix: { value0: { value0: nss, value1: i_max }, value1: j_max } // IntMatrix
   }
) {
   const cellFillSelected = 'Yellow'
   const w = 30, h = 30
   const div = d3.select('#' + id)
   const [width, height] = [w * j_max + strokeWidth, h * i_max + strokeWidth]
   const hMargin = w / 2
   const vMargin = h / 2

   const svg = div.append('svg')
                  .attr('width', width + hMargin)
                  .attr('height', height + vMargin)

   // group for each row
   const grp = svg.selectAll('g')
      .data(nss)
      .enter()
      .append('g')
      .attr('transform', (_, i) => `translate(${strokeWidth / 2 + hMargin / 2}, ${h * i + strokeWidth / 2 + vMargin})`)

   const rect = grp.selectAll('rect')
                     .data(d => d)
                     .enter()

   rect.append('rect')
         .attr('x', (_, j) => w * j)
         .attr('width', w)
         .attr('height', h)
         .attr('fill', d => d.value1 ? cellFillSelected : cellFillDefault)
         .attr('stroke', cellStroke)
         .attr('stroke-width', strokeWidth)

   rect.append('text')
         .text(d => d.value0)
         .attr('x', (_, j) => w * (j + 0.5))
         .attr('y', 0.5 * h)
         .attr('fill', cellTextFill)
         .attr('font-family', fontFamily)
         .attr('font-size', cellFontSize)
         .attr('text-anchor', 'middle')
         .attr('dominant-baseline', 'middle')

   svg.append('text')
      .text(title)
      .attr('x', hMargin / 2)
      .attr('y', vMargin / 2)
      .attr('fill', titleTextFill)
      .attr('font-family', fontFamily)
      .attr('font-size', titleFontSize)
      .attr('dominant-baseline', 'middle')
      .attr('text-anchor', 'left')
}

function drawBarChart (
   id, {
      caption,   // String
      data_,     // Array BarChartRecord
   }
) {
   // set the dimensions and margins of the graph
   const margin = {top: 15, right: 0, bottom: 30, left: 30},
         width = 200 - margin.left - margin.right,
         height = 175 - margin.top - margin.bottom

   // append the svg object to the body of the page
   const svg = d3.select('#' + id)
      .append('svg')
         .attr('width', width + margin.left + margin.right)
         .attr('height', height + margin.top + margin.bottom)
      .append('g')
         .attr('transform', `translate(${margin.left}, ${margin.top})`)

   const tip = d3tip.default()
      .attr('class', 'd3-tip')
      .offset([0, 0])
      .html((ev, d) => {
         return d.y.value0
      })

   svg.call(tip)

   // x-axis
   const x = d3.scaleBand()
      .range([ 0, width ])
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
      .range([ height, 0])
   const tick_every = nearest / 2
   const yAxis = d3.axisLeft(y)
      .tickValues(Array.from(Array(y_max / tick_every + 1).keys()).map(n => n * tick_every))
   svg.append('g')
      .call(yAxis)

   // Bars
   const barFill = '#dcdcdc'
   svg.selectAll('rect')
      .data(data_)
      .enter()
      .append('rect')
         .attr('id', 'mouse-cursor')
         .attr('x', d => x(d.x.value0))
         .attr('y', d => y(d.y.value0 + 1))  // ouch: bars overplot x-axis!
         .attr('width', x.bandwidth())
         .attr('height', d => height - y(d.y.value0))
         .attr('fill', d => d.y.value1 ? colorShade(barFill, -40) : barFill)
         .attr('stroke', d => d.y.value1 ? 'coral' : '')
         .on('mouseover', tip.show)
         .on('mouseout', tip.hide)
}

function drawLineChart (
   id, {
      caption,   // String
      data_,     // Array BarChartRecord
   }
) {
}

// https://stackoverflow.com/questions/5560248
function colorShade (col, amt) {
   col = col.replace(/^#/, '')
   if (col.length === 3) col = col[0] + col[0] + col[1] + col[1] + col[2] + col[2]

   let [r, g, b] = col.match(/.{2}/g);
   ([r, g, b] = [parseInt(r, 16) + amt, parseInt(g, 16) + amt, parseInt(b, 16) + amt])

   r = Math.max(Math.min(255, r), 0).toString(16)
   g = Math.max(Math.min(255, g), 0).toString(16)
   b = Math.max(Math.min(255, b), 0).toString(16)

   const rr = (r.length < 2 ? '0' : '') + r
   const gg = (g.length < 2 ? '0' : '') + g
   const bb = (b.length < 2 ? '0' : '') + b

   return `#${rr}${gg}${bb}`
 }


// any record type with only primitive fields -> boolean
function isUsed (r) {
   return Object.keys(r).some(k => r[k].value1)
}

// Generic to all tables.
function drawTable (
   id, {
      title,               // String
      table                // Array of any record type with only primitive fields
   }) {
   table = table.filter(r => isUsed(r))
   const cellFill = '#ffffff'
   const HTMLtable = d3.select('#' + id)
      .append('table')
   const colNames = Object.keys(table[0])
   HTMLtable.append('thead')
      .append('tr')
      .selectAll('th')
      .data(colNames).enter()
      .append('th')
      .text(d => d)
   const rows = HTMLtable.append('tbody').selectAll('tr')
      .data(table).enter()
      .append('tr')
   rows.selectAll('td')
      .data(d => colNames.map(k => { return { 'value': d[k], 'name': k } }))
      .enter()
      .append('td')
      .attr('data-th', d => d.name)
      .attr('class', d => d.value.value1 ? 'cell-selected' : null)
      .attr('bgcolor', d => d.value.value1 ? colorShade(cellFill, -40) : cellFill)
      .text(d => d.value.value0)
}

function className (o) {
   return o.constructor.name
}

// Figs -> Effect Unit
function drawFig ({ divId, subfigs }) {
   return () => {
      for (const fig of subfigs) {
         // Bit horrible but will do for now.
         if (className(fig) == "EnergyTable") {
            drawTable(divId, fig.value0)
         }
         else
         if (className(fig) == "BarChartFig") {
            drawBarChart(divId, fig.value0)
         }
         else
         if (className(fig) == "LineChartFig") {
            drawLineChart(divId, fig.value0)
         }
         else
         if (className(fig) == "MatrixFig") {
            drawMatrix(divId, fig.value0)
         }
         else {
            throw new Error(`Figure type '${className(fig)}' not recognised.`)
         }
      }
   }
}

// Currently unused.
function saveImage (svg) {
   const svg_xml = (new XMLSerializer()).serializeToString(svg),
         blob = new Blob([svg_xml], { type:'image/svg+xml;charset=utf-8' }),
         url = window.URL.createObjectURL(blob),
         { width, height } = svg.getBBox()

   const img = new Image()
   img.width = width
   img.height = height

   img.onload = function() {
       const canvas = document.createElement('canvas')
       canvas.width = width
       canvas.height = height

       const ctx = canvas.getContext('2d')
       ctx.drawImage(img, 0, 0, width, height)

       window.URL.revokeObjectURL(url)
       const dataURL = canvas.toDataURL('image/png')
       download(canvas, dataURL, "image.png")
   }
   img.src = url
}

function download (parent, dataURL, name) {
   const a = document.createElement('a')
   a.download = name
   a.style.opacity = '0'
   parent.append(a)
   a.href = dataURL
   a.click()
   a.remove()
 }

function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

exports.drawFig = drawFig
