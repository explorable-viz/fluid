"use strict"

import d3 from "d3";
import shared from "/src/app/Shared";

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
            .attr('bgcolor', d => d.value.value1 ? shared.colorShade(cellFill, -40) : cellFill)
            .text(d => d.value.value0)
            .on('mouseover', (e, d) =>
               listener(e)
            )
      }
   }
}

export var drawTable = shared.curry4(drawTable);
