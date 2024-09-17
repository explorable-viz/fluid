"use strict"

import * as d3 from "d3"

function prim (v) {
   if (isNaN(parseFloat(v._1))) {
      return v._1
   } else {
      return +parseFloat(v._1).toFixed(2)
   }
}

function setSelState_ (
   { title, table },
   { record_isDisplayable, cell_selClassesFor, val_selState, hasRightBorder, hasBottomBorder },
   selListener,
   rootElement
) {
   return () => {
      // This definition available PureScript-side
      const selClasses =
         "selected-primary-transient selected-secondary-transient selected-primary-persistent selected-secondary-persistent inert"

      rootElement.selectAll('.table-cell').each(function (cell) {
         if (cell.i != -1 && cell.j != -1) {
            const sel = val_selState(table[cell.i][cell.j])
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
         hide = !record_isDisplayable(table[i])
         if (hide)
            hidden++
         d3.select(this) // won't work inside arrow function :/
            .classed('hidden', hide)
      })

      rootElement.select('.table-caption')
         .text(title + ' (' + (table.length - hidden) + ' of ' + table.length + ')' )

      rootElement.selectAll('.table-cell').each(function (cell) {
         d3.select(this)
            .classed('has-right-border', hasRightBorder(table)(cell.i)(cell.j))
            .classed('has-bottom-border', hasBottomBorder(table)(cell.i)(cell.j))
      })
   }
}

function createRootElement_ (
   view,
   viewHelpers,
   div,
   childId
) {
   return () => {
      let { colNames, table } = view
      // These definitions will be available on PureScript side
      const rowKey = "__n"
      function val_val (v) {
         return v._2
      }
      colNames = [rowKey, ...colNames]
      rootElement = div
         .append('table')
         .attr('id', childId)

      rootElement.append('caption')
         .attr('x', 0)
         .attr('y', 0)
         .attr('class', 'title-text table-caption')
         .attr('dominant-baseline', 'middle')
         .attr('text-anchor', 'left')

      rootElement.append('thead')
         .append('tr')
         .selectAll('th')
            .data(colNames.map((colName, j) => ({ i: -1, j: j - 1, colName })))
            .enter()
            .append('th')
            .text(cell => cell.colName == rowKey ? (view.filter ? "▸" : "▾" ) : cell.colName)
            .classed('filter-toggle toggle-button', colName => colName == rowKey)
            .attr('class', 'table-cell')

      const rows = rootElement
         .append('tbody')
         .selectAll('tr')
            .data(table.map((row, i) => ({ i, vals: [i + 1, ...row] }))) // data rows have 0-based index, but displayed row numbers start with 1
            .enter()
            .append('tr')
            .attr('class', 'table-row')

      rows.selectAll('td')
         .data(({ i, vals }) =>
            vals.map((val, j) => ({ i, j: j - 1, value: val, colName: colNames[j] }))) // field for row number has j = -1
         .enter()
         .append('td')
         .attr('class', 'table-cell')
         .style('border-top', "1px solid transparent")
         .style('border-left', "1px solid transparent")
         .style('border-right', ({ j }) => j == colNames.length - 2 ? "1px solid transparent" : null)
         .style('border-bottom', ({ i }) => i == table.length -1 ? "1px solid transparent" : null)
         .text(({ colName, value }) => colName == rowKey ? value : prim(val_val(value)))

      return rootElement
   }
}

export const setSelState = x1 => x2 => x3 => x4 => setSelState_(x1, x2, x3, x4)
export const createRootElement = x1 => x2 => x3 => x4 => createRootElement_(x1, x2, x3, x4)
