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
   { cell_selClassesFor, rowKey, record_isUsed, record_isReactive,record_isDisplayable, val_selState },
   filterToggleListener,
   {
      selClasses,
   },
   rootElement,
   { title, filter, table },
   selListener
) {
   rootElement.selectAll('.table-cell').each(function (cell) {
      if (cell.colName != rowKey) {
         const sel = val_selState(table[cell.i][cell.colName])
         d3.select(this) // won't work inside arrow function :/
            .classed(selClasses, false)
            .classed(cell_selClassesFor(cell.colName)(sel), true)
            .on('mousedown', e => { selListener(e) })
            .on('mouseenter', e => { selListener(e) })
            .on('mouseleave', e => { selListener(e) })
      }
   })

   let hidden = 0
   rootElement.selectAll('.table-row').each(function ({ i }) {
      hide = !record_isDisplayable(table[i])(filter)
      if (hide)
         hidden++
      d3.select(this) // won't work inside arrow function :/
         .classed('hidden', hide)
   })

   rootElement.select('.table-caption')
      .text(title + ' (' + (table.length - hidden) + ' of ' + table.length + ')' )
   rootElement.select('.filter-toggle')
      .on('mousedown', e => { filterToggleListener(e) })
}

function drawTable_ (
   tableViewHelpers,
   filterToggleListener,
   uiHelpers,
   {
      divId,
      suffix,
      view
   },
   selListener
) {
   return () => {
      const { rowKey, val_val } = tableViewHelpers
      let { table } = view
      const childId = divId + '-' + suffix

      const div = d3.select('#' + divId)
      if (div.empty()) {
         console.error('Unable to insert figure: no div found with id ' + divId)
         return
      }

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
               .text(colName => colName == rowKey ? (view.filter ? "▸" : "▾" ) : colName)
               .classed('filter-toggle toggle-button', colName => colName == rowKey)

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

      setSelState(tableViewHelpers, filterToggleListener, uiHelpers, rootElement, view, selListener)
   }
}

export var drawTable = x1 => x2 => x3 => x4 => x5 => drawTable_(x1, x2, x3, x4, x5)
