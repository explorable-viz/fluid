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

// String -> MatrixFig -> Effect Unit
function drawMatrix (
   id, {
      title,
      cellFillSelected,
      matrix: { value0: { value0: nss, value1: i_max }, value1: j_max }
   }
) {
   return () => {
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
}

// String -> Array MatrixFig -> Effect Unit
function drawFigure (id, figs) {
   return () => {
      for (fig in figs) {
         drawMatrix(id, fig)()
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

exports.drawFigure = curry4(drawFigure)
