"use strict"

import * as d3 from "d3"

// Difference from other JS mappings: values in each cell are not "unpacked" to Selectable but remain as Val
function Val_val(x) {
   return x._2
}

function prim (v) {
   if (isNaN(parseFloat(v._1))) {
      return v._1
   } else {
      return +parseFloat(v._1).toFixed(2)
   }
}

// Generic to all tables.
function drawTable_ (
   {
      uiHelpers,
      divId,
      suffix,
      view: {
         title,   // String
         filter,  // Boolean
         table    // Homogeneous array of records with fields of primitive type
      }
   },
   listener
) {
   return () => {
      const { tableViewHelpers: { record_isUsed, cell_classes } } = uiHelpers
      const childId = divId + '-' + suffix
      const div = d3.select('#' + divId)

      indexKey = "__n"
      table = table.map((r, n) => { return {[ indexKey ]: n + 1, ...r} })

      const unfilteredLength = table.length
      div.selectAll('#' + childId).remove()
      if (filter) {
         table = table.filter(r => record_isUsed(r))
      }

      if (table.length > 0) {
         const HTMLtable = div
            .append('table')
            .attr('id', childId)

         const colNames = Object.keys(table[0]).sort()

         HTMLtable.append('caption')
            .text(title + ' (' + table.length + ' of ' + unfilteredLength + ')' )
            .attr('x', 0)
            .attr('y', 0)
            .attr('class', 'title-text table-caption')
            .attr('dominant-baseline', 'middle')
            .attr('text-anchor', 'left')

         const tableHead = HTMLtable.append('thead')
         tableHead
            .append('tr')
            .selectAll('th')
            .data(colNames)
            .enter()
            .append('th')
            .text(d => d == indexKey ? (filter ? "▸" : "▾" ) : d)

         const rows = HTMLtable
            .append('tbody')
            .selectAll('tr')
            .data(table)
            .enter()
            .append('tr')

         rows.selectAll('td')
            .data(d => colNames.map(
               k => { return { [ indexKey ]: d[indexKey], 'value': d[k], 'name': k } })
            )
            .enter()
            .append('td')
            .attr('data-th', d => d.name)
            .attr('class', d => cell_classes(d.name)(d.value))
            .text(d => d.name != indexKey ? prim(Val_val(d.value)) : d.value)
            .on('mousedown', e => listener(e))

         sel = d3.select("th")
         sel.on("mouseover", _ => console.log("TODO: toggle filter state persistently"))
      }
   }
}

export var drawTable = x1 => x2 => drawTable_(x1, x2)
