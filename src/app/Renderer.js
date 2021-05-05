"use strict"

const d3 = require("d3")

function drawMatrix (
   nss,     // Array (Array (Int × Bool))
   i_max,   // Int
   j_max    // Int
) {
   return () => {
      const w = 30, h = 30, gap = 1.15
      const div = d3.select('#app-root'),
            svg = div.append('svg')
                     .attr('width', w * j_max * gap)
                     .attr('height', h * i_max * gap)
                     .attr('fill', 'lightgray')
      const grp = svg.selectAll('g')
         .data(nss)
         .enter()
         .append('g')
         .attr('transform', (_, i) => "translate(0, " + h * gap * i + ")")

      const rect = grp.selectAll('rect')
                      .data(d => d)
                      .enter()

      rect.append('rect')
          .attr('x', (_, j) => w * gap * j)
          .attr('width', w)
          .attr('height', h)
          .attr('fill', d => d.value1 ? 'green' : 'lightgray')

      rect.append('text')
          .attr('x', (_, j) => w * gap * j)
          .attr('y', 0.5 * h)
          .attr('fill', 'black')
          .text(d => d.value0)

      wibble(svg.node())
   }
}

function wibble (svg) {
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
       const canvasData = canvas.toDataURL('image/png'),
             a = document.getElementById('imgId')
       a.download = "export_" + Date.now() + ".png"
       a.href = canvasdata;
   }
   img.src = url
}

// not using this yet
function download (parent, url, name) {
   const link = document.createElement('a')
   link.download = name
   link.style.opacity = '0'
   parent.append(link)
   link.href = url
   link.click()
   link.remove()
 }

function curry2 (f) {
   return x => y => f(x, y)
}

function curry3 (f) {
   return x => y => z => f(x, y, z)
}

exports.drawMatrix = curry3(drawMatrix)
