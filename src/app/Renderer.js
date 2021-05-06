"use strict"

const d3 = require("d3")

const cellFillDefault   = 'White',
      cellFillSelected  = 'PaleGreen',
      cellStroke        = 'DarkGray'

// String -> MatrixRep' -> Effect Unit
function drawMatrix (id, { value0: { value0: nss, value1: i_max }, value1: j_max }) {
   return () => {
      const w = 30, h = 30
      const div = d3.select('#' + id),
            svg = div.append('svg')
                     .attr('width', w * j_max)
                     .attr('height', h * i_max)

      // group for each row
      const grp = svg.selectAll('g')
         .data(nss)
         .enter()
         .append('g')
         .attr('transform', (_, i) => "translate(0, " + h * i + ")")

      const rect = grp.selectAll('rect')
                      .data(d => d)
                      .enter()

      rect.append('rect')
          .attr('x', (_, j) => w * j)
          .attr('width', w)
          .attr('height', h)
          .attr('fill', d => d.value1 ? cellFillSelected : cellFillDefault)
          .attr('stroke', cellStroke)
          .attr('stroke-width', '0.5')

      rect.append('text')
          .attr('x', (_, j) => w * j)
          .attr('y', 0.5 * h)
          .attr('fill', 'black')
          .attr('font-family', 'Roboto, sans-serif')
          .attr('font-size', '10pt')
          .text(d => d.value0)

      grp.selectAll('line')
         .data([1])
         .enter()
         .append('line')
         .attr('stroke', cellStroke)
         .attr('stroke-width', '5')
         .attr('x1', 0)
         .attr('y1', 0)
         .attr('x2', w * j_max)
         .attr('y2', 0)
   }
}

// String -> MatrixRep' -> MatrixRep' -> MatrixRep' -> Effect Unit
function drawFigure (id, m1, m2, m3) {
   return () => {
      drawMatrix(id, m1)()
      drawMatrix(id, m2)()
      drawMatrix(id, m3)()
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

exports.drawFigure = curry4(drawFigure)
