"use strict"

import * as d3 from "d3"

d3.selection.prototype.attrs = function(m) {
   for (const k in m) {
      this.attr(k, m[k])
   }
   return this
}

function setSelState (
   { },
   {
      selState,
      selClasses,
      selClassesFor,
      join
   },
   div,
   view,
   selListener
) {
   div.selectAll('span').each(function (textElem) {
      var sel
      if (textElem.conts.tag == "Left") {
         console.log("SetSelState left, View: ", view[textElem.i])
         sel = selState((view[textElem.i])._1)
      }
      else {
         console.log("TextElem: ", textElem.conts)
         console.log("View: ", (view[textElem.i]))
         sel = (view[textElem.i])._1._1._1
      }
      d3.select(this)
         .classed(selClasses, false)
         .classed(selClassesFor(sel), true)
         .on('mousedown', e => { selListener(e) })
         .on('mouseenter', e =>{ selListener(e)})
         .on('mouseleave', e => { selListener(e)})
   })
}

function drawLinkedText_ (
   linkedTextHelpers,
   uiHelpers,
   {
      divId,
      suffix,
      view
   },
   selListener
) {
   return () => {
      const div = d3.select('#' + divId)
      const childId = divId + '-' + suffix
      let rootElement = div.selectAll('#' + childId)
      if (rootElement.empty()) {
         rootElement = div
            .append("div")
            .attr("id", childId)
            .text(view._1)
            .attr('class', 'linked-text-parent')
         var tooltip = div
            .append("div")
            .style("opacity", 0)
            .attr("class", "tooltip")
            .style("position", "absolute")
            .style("background-color", "white")
            .style("border", "solid")
            .style("border-color", "#40BFA0")
            .style("border-width", "2px")
            .style("border-radius", "5px")
            .style("padding", "5px")
         rootElement.selectAll('span')
            .data([...view.entries()].map(([i, conts]) => { console.log("Contents:", conts); return {i, conts}}))
            .enter()
            .append('span')
            .attr('id', childId)
            .text(d => chooseText(d))
            .attr('class', 'linked-text')
            .on("mousemove", (e, d) => { tooltip.html("This cell is explained by: " + explainChoice(d)).style("left", (e.pageX+70)+"px").style("top", e.pageY + "px") })
            .on("mouseover", (e, d)=> { tooltip.style("opacity", 1)} )
            .on("mouseout", d=> { tooltip.style("opacity", 0)})
      }
      setSelState(linkedTextHelpers, uiHelpers, rootElement, view, selListener)
   }
}


// function test() {
//    {
//       "tag":"Right",
//       "_1": {
//          "tag":"Tuple",
//          "_1":{
//             "tag":"Explanation",
//             "_1":"probAsText (computeProb (ssp119.lowLate, ssp119.highLate) 2.0) ",
//             "_2":{
//                "tag":"Val",
//                "_1":{
//                   "tag":"Reactive",
//                   "_1":{"persistent":{"tag":"None"},"transient":{"tag":"None"}}
//                   },
//                "_2":{
//                   "tag":"Str",
//                   "_1":"exceptionally unlikely"
//                }}},
//          "_2":{"tag":"Inert"}}}

// }

function chooseText(d) {
   if (d.conts.tag == "Left") {
      console.log("chooseText Left: ", d.conts)
      return d.conts._1._1
   } else {
      console.log("chooseText Right: ", d.conts)
      return d.conts._1._1._3._2._1
   }
}

function explainChoice(d) {
   console.log("Explanation: ", d.conts)
   if (d.conts.tag == "Left")
   {
      console.log("Left")
      return d.conts._1._1
   } else {
      console.log("Right")
      console.log(d.conts._1)
      return d.conts._1._1._2
   }  
}

export var drawLinkedText = x1 => x2 => x3 => x4 => drawLinkedText_(x1, x2, x3, x4)
