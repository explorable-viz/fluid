"use strict"

import * as d3 from "d3";

// This prelude currently duplicated across all FFI implementations.
function curry2(f) {
   return x1 => x2 => f(x1, x2)
}

function curry3(f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4(f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
}

// https://stackoverflow.com/questions/5560248
function colorShade(col, amt) {
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
   id,
   childIndex,
   {
      title,   // String
      table    // Array of any record type with only primitive fields
   },
   listener
) {
   return () => {
      const childId = id + '-' + childIndex
      const cellFill = '#ffffff'
      const div = d3.select('#' + id)

      div.selectAll('#' + childId).remove()
      table = table.filter(r => isUsed(r))

      if (table.length > 0) {
         const HTMLtable = div
            .append('table')
            .attr('id', childId)
         const colNames = Object.keys(table[0])

         HTMLtable.append('thead')
            .append('tr')
            .selectAll('th')
            .data(colNames)
            .enter()
            .append('th')
            .text(d => d)

         const rows = HTMLtable
            .append('tbody')
            .selectAll('tr')
            .data(table)
            .enter()
            .append('tr')

         rows.selectAll('td')
            .data(d => colNames.map(k => { return { 'value': d[k], 'name': k } }))
            .enter()
            .append('td')
            .attr('data-th', d => d.name)
            .attr('class', d => d.value.value1 ? 'cell-selected' : null)
            .attr('bgcolor', d => d.value.value1 ? colorShade(cellFill, -40) : cellFill)
            .text(d => d.value.value0)
            .on('mouseover', (e, d) =>
               listener(e)
            )
      }
   }
}

export var drawTable = curry4(drawTable);
