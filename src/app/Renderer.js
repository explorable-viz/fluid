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

      wibble()
   }
}

function wibble () {
   var svg = document.getElementsByTagName('svg')[0];
   var svg_xml = (new XMLSerializer()).serializeToString(svg),
      blob = new Blob([svg_xml], {type:'image/svg+xml;charset=utf-8'}),
      url = window.URL.createObjectURL(blob);

   var img = new Image();
   img.width = 730;
   img.height = 300;
   img.onload = function() {
      var canvas = document.createElement('canvas');
      canvas.width = 730;
      canvas.height = 300;

      var ctx = canvas.getContext('2d');
      ctx.drawImage(img, 0, 0, 730, 300);

      window.URL.revokeObjectURL(url);
      var canvasdata = canvas.toDataURL('image/png');
      var a = document.getElementById('imgId');
      a.download = "export_" + Date.now() + ".png";
      a.href=canvasdata;
   }
   img.src = url
}

function curry2 (f) {
   return x => y => f(x, y)
}

function curry3 (f) {
   return x => y => z => f(x, y, z)
}

exports.drawMatrix = curry3(drawMatrix)
