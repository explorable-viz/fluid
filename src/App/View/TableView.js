"use strict"

import * as d3 from "d3"

function setSelState_ (
   { title, rows },
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
            const sel = val_selState(rows[cell.i][cell.j])
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
         hide = !record_isDisplayable(rows[i])
         if (hide)
            hidden++
         d3.select(this) // won't work inside arrow function :/
            .classed('hidden', hide)
      })

      rootElement.select('.table-caption')
         .text(title + ' (' + (rows.length - hidden) + ' of ' + rows.length + ')' )

      rootElement.selectAll('.table-cell').each(function (cell) {
         d3.select(this)
            .classed('has-right-border', hasRightBorder(rows)(cell.i)(cell.j))
            .classed('has-bottom-border', hasBottomBorder(rows)(cell.i)(cell.j))
      })
   }
}

export const setSelState = x1 => x2 => x3 => x4 => setSelState_(x1, x2, x3, x4)
