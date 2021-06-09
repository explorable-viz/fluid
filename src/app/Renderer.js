"use strict"

const d3 = require("d3")

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
      cellFillSelected,                                                 // String
      matrix: { value0: { value0: nss, value1: i_max }, value1: j_max } // IntMatrix
   }
) {
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
      title,   // String
      data,    // Array BarChartRecord
   }
) {
   // set the dimensions and margins of the graph
   var margin = {top: 30, right: 30, bottom: 70, left: 60},
      width = 460 - margin.left - margin.right,
      height = 400 - margin.top - margin.bottom;

   // append the svg object to the body of the page
   const svg = d3.select('#' + id)
      .append('svg')
         .attr('width', width + margin.left + margin.right)
         .attr('height', height + margin.top + margin.bottom)
      .append('g')
         .attr('transform', `translate(${margin.left}, ${margin.top})`)

   // x-axis
   const x = d3.scaleBand()
      .range([ 0, width ])
      .domain(data.map(d => d.x.value0))
      .padding(0.2)
   svg.append('g')
      .attr('transform', "translate(0," + height + ")")
      .call(d3.axisBottom(x))
      .selectAll('text')
         .attr('transform', "translate(-10,0)rotate(-45)")
         .style('text-anchor', 'end')

   // y-axis
   const nearest = 100,
         y_max = Math.ceil(Math.max(...data.map(d => d.y.value0)) / nearest) * nearest
   const y = d3.scaleLinear()
      .domain([0, y_max])
      .range([ height, 0])
   svg.append('g')
      .call(d3.axisLeft(y))

   // Bars
   svg.selectAll('rect')
      .data(data)
      .enter()
      .append('rect')
         .attr('x', d => x(d.x.value0))
         .attr('y', d => y(d.y.value0))
         .attr('width', x.bandwidth())
         .attr('height', d => height - y(d.Value))
         .attr('fill', "#69b3a2")
}

// Generic to all tables.
function drawTable (
   id, {
      title,               // String
      cellFillSelected,    // String
      table                // Array of any record type
   }) {
   const HTMLtable = d3.select('#' + id)
      .append('table')
   const colNames = Object.keys(table[0])
   HTMLtable.append('thead').append('tr').selectAll('th')
      .data(colNames).enter()
      .append('th')
      .text(d => d)
   const rows = HTMLtable.append('tbody').selectAll('tr')
      .data(table).enter()
      .append('tr')
   rows.selectAll('td')
      .data(d => colNames.map(k => { return { 'value': d[k].value0, 'name': k } }))
      .enter()
      .append('td')
      .attr('data-th', d => d.name)
      .text(d => d.value)
}

function className (o) {
   return o.constructor.name
}

// String -> Array Fig -> Effect Unit
function drawFigure (id, figs) {
   return () => {
      for (const fig of figs) {
         // Bit horrible but will do for now.
         if (className(fig) == "EnergyTable") {
            drawTable(id, fig.value0)
         }
         else
         if (className(fig) == "BarChart") {
            drawBarChart(id, fig.value0)
         }
         else
         if (className(fig) == "LineChart") {
            drawBarChart(id, fig.value0)
         }
         else
         if (className(fig) == "MatrixFig") {
            drawMatrix(id, fig.value0)
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

function curry3 (f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4 (f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
}

exports.drawFigure = curry2(drawFigure)
