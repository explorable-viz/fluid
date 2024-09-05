"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

d3.selection.prototype.attrFuns = function(m) {
   for (const k in m) {
      this.attr(k, d => m[k](d))
   }
   return this
}

function computed(element, prop) {
   return window.getComputedStyle(element, null).getPropertyValue(prop);
}

// should be always defined
function canvasFont(el) {
   return `${computed(el, 'font-weight')} ${computed(el, 'font-size')} ${computed(el, 'font-family')}`
}

// https://stackoverflow.com/questions/118241
// Pure; side-effects should be unobservable
// Could assume existing div in document set up for this purpose, rather than creating here
export function textWidth (text) {
   const div = document.createElement('div')
   div.textContent = text
   div.classList.add('legend-text')
   div.style.visibility = 'hidden'
   document.body.appendChild(div)

   // re-use canvas object
   const canvas = textWidth.canvas || (textWidth.canvas = document.createElement("canvas"))
   const context = canvas.getContext("2d")
   context.font = canvasFont(div)
   const width = context.measureText(text).width
   div.remove()
   return Math.floor(width)
}

function createChild_ (parent, elementType, attrs) {
   return () => {
      return parent.append(elementType).attrs(attrs)
   }
}

function createChildren_ (parent, elementType, data, attrFuns) {
   return () => {
      return parent
         .selectAll(elementType)
         .data(data)
         .enter()
         .append(elementType)
         .attrFuns(attrFuns)
   }
}

// Maybe this can be pure?
function line_ (to, points) {
   return () => {
      const line = d3.line()
         .x(d => to.x(d.x))
         .y(d => to.y(d.y))
      return line(points)
   }
}

function xAxis_ (to, ticks, element) {
   return () => {
      return element.call(d3.axisBottom(to.x).ticks(ticks.x).tickFormat(d3.format('d')))
   }
}

function yAxis_ (to, ticks, element) {
   return () => {
      return element.call(d3.axisLeft(to.y).tickSizeOuter(0).ticks(ticks.y).tickFormat(d3.format('.1f')))
   }
}

export var createChild = x1 => x2 => x3 => createChild_(x1, x2, x3)
export var createChildren = x1 => x2 => x3 => x4 => createChildren_(x1, x2, x3, x4)
export var line = x1 => x2 => line_(x1, x2)
export var xAxis = x1 => x2 => x3 => xAxis_(x1, x2, x3)
export var yAxis = x1 => x2 => x3 => yAxis_(x1, x2, x3)
export var scaleLinear = x1 => x2 => d3.scaleLinear().domain([x1.min, x1.max]).range([x2.min, x2.max])
