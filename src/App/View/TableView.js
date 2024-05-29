"use strict"

import * as d3 from "d3"

function prim (v) {
   if (isNaN(parseFloat(v._1))) {
      return v._1
   } else {
      return +parseFloat(v._1).toFixed(2)
   }
}

function setSelState (
   {
      selClasses,
      tableView: { cell_selClass, rowKey, record_isUsed, val_selState }
   },
   rootElement,
   { title, table },
   listener
) {
   rootElement.selectAll('.table-cell').each(function (cell) {
      if (cell.colName != rowKey) {
         const sel = val_selState(table[cell.i][cell.colName])
         d3.select(this) // won't work inside arrow function :/
            .classed(selClasses, false)
            .classed(cell_selClass(cell.colName)(sel), true)
            .on('mousedown', e => { listener(e) })
            .on('mouseenter', e => { listener(e) })
            .on('mouseleave', e => { listener(e) })
      }
   })
   let hidden = 0
   rootElement.selectAll('.table-row').each(function ({ i }) {
      hide = !record_isUsed(table[i])
      if (hide) hidden++
      d3.select(this) // won't work inside arrow function :/
         .classed('hidden', hide)
   })
   rootElement.select('.table-caption')
      .text(title + ' (' + (table.length - hidden) + ' of ' + table.length + ')' )
}

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
      const { tableView: { rowKey, val_val } } = uiHelpers
      const childId = divId + '-' + suffix
      const div = d3.select('#' + divId)

      table = table.map((r, n) => { return {[ rowKey ]: n + 1, ...r} })
      const colNames = Object.keys(table[0]).sort()

      let rootElement = div.selectAll('#' + childId)

      if (rootElement.empty()) {
         rootElement = div
            .append('table')
            .attr('id', childId)

         rootElement.append('caption')
            .attr('x', 0)
            .attr('y', 0)
            .attr('class', 'title-text table-caption')
            .attr('dominant-baseline', 'middle')
            .attr('text-anchor', 'left')

         const tableHead = rootElement.append('thead')
         tableHead
            .append('tr')
            .selectAll('th')
            .data(colNames)
            .enter()
            .append('th')
            .text(colName => colName == rowKey ? (filter ? "▸" : "▾" ) : colName)

         const rows = rootElement
            .append('tbody')
            .selectAll('tr')
            .data([...table.entries()].map((([i, row]) => { return { i, row } })))
            .enter()
            .append('tr')
            .attr('class', 'table-row')

         rows.selectAll('td')
            .data(({ i, row }) => colNames.map(colName => {
               return { [rowKey]: row[rowKey], i, colName, value: row[colName] }
            }))
            .enter()
            .append('td')
            .attr('class', 'table-cell')
            .text(cell => cell.colName == rowKey ? cell.value : prim(val_val(cell.value)))
      }

      setSelState(uiHelpers, rootElement, { title, table }, listener)
   }
}

export var drawTable = x1 => x2 => drawTable_(x1, x2)
