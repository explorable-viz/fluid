"use strict"

function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

function curry3 (f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4 (f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
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

exports.colorShade = colorShade
exports.curry2 = curry2
exports.curry3 = curry3
exports.saveImage = saveImage
