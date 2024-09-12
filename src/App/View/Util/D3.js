"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

/* Not sure I need this; equivalent to attrs, once type information is disregarded */
d3.selection.prototype.attrFuns = function(m) {
   for (const k in m) {
      this.attr(k, d => m[k](d))
   }
   return this
}

d3.selection.prototype.styles = function(m) {
   for (const k in m) {
      this.style(k, m[k])
   }
   return this
}

function computed (element, prop) {
   return window.getComputedStyle(element, null).getPropertyValue(prop);
}

// should be always defined
function canvasFont (el) {
   return `${computed(el, 'font-weight')} ${computed(el, 'font-size')} ${computed(el, 'font-family')}`
}

// Could assume a div in document set up for this purpose. Pure; side-effects should be unobservable.
// Not especially reliable as might not inherit in situ styling that the actual text will
export function textDimensions (class_) {
   return text => {
      const div = document.createElement('div')
      div.textContent = text
      div.classList.add(class_)
      div.style.visibility = 'hidden'
      document.body.appendChild(div)

      const canvas = textDimensions.canvas || (textDimensions.canvas = document.createElement("canvas")) // re-use canvas
      const context = canvas.getContext("2d")
      context.font = canvasFont(div)
      const width = Math.ceil(context.measureText(text).width)
      const height = Math.ceil(div.offsetHeight)
      div.remove()
      return { width, height }
   }
}

export function createChild (parent) {
   return elementType => {
      return attrs => {
         return () => {
            return parent.append(elementType).attrs(attrs)
         }
      }
   }
}

export function forEach_createChild (parent) {
   return elementType => {
      return attrFuns => {
         return () => {
            return parent.append(elementType).attrFuns(attrFuns)
         }
      }
   }
}

export function createChildren (parent) {
   return elementType => {
      return class_ => {
         return data => {
            return attrFuns => {
               return () => {
                  return parent
                     .selectAll("." + class_)
                     .data(data)
                     .enter()
                     .append(elementType)
                     .classed(class_, true)
                     .attrFuns(attrFuns)
               }
            }
         }
      }
   }
}

export function remove (element) {
   return () => {
      element.remove()
   }
}

export function line (to) {
   return points => {
      const line = d3.line()
         .x(d => to.x(d.x))
         .y(d => to.y(d.y))
      return line(points)
   }
}

export function xAxis (to) {
   return ticks => {
      return parent => {
         return () => {
            return parent.call(d3.axisBottom(to.x).tickValues(ticks).tickFormat(d3.format('d')))
         }
      }
   }
}

export function yAxis (to) {
   return nTicks => {
      return parent => {
         return () => {
            return parent.call(d3.axisLeft(to.y).tickSizeOuter(0).ticks(nTicks).tickFormat(d3.format('.1f')))
         }
      }
   }
}

export function setText (string) {
   return element => {
      return () => {
         return element.text(string)
      }
   }
}

export function forEach_setText (f) {
   return setText(f)
}

const colScale = d3.scaleOrdinal(d3.schemePastel1) // stateful but purify by allocating once

export function nameCol (key) {
   return keys => {
      return colScale(keys.indexOf(key))
   }
}

export function dimensions (sel) {
   return () => {
      if (sel.nodes().length != 1) {
         throw "Expected singleton selection"
      }
      [ node ] = sel.nodes()
      let { width, height } = node.getBBox()
      return { width: Math.ceil(width), height: Math.ceil(height) }
   }
}

export function selectAll (sel) {
   return selector => {
      return () => {
         return sel.selectAll(selector)
      }
   }
}

export function attrs (sel) {
   return attrs => {
      return () => {
         return sel.attrs(attrs)
      }
   }
}

export function styles (sel) {
   return styles => {
      return () => {
         return sel.styles(styles)
      }
   }
}

export function scaleLinear (x1) {
   return x2 => {
      return d3.scaleLinear().domain([x1.min, x1.max]).range([x2.min, x2.max])
   }
}
