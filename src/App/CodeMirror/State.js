"use strict"

"use strict"

import * as d3 from "d3"

import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"

let startState = EditorState.create({
  doc: "Hello World",
  extensions: [keymap.of(defaultKeymap)]
})

function addEditorView_ (id) {
   return () => {
      const childId = id + '-' + 1
      const div = d3.select('#' + id)
      const svg = div
         .append('svg')
         .attr('id', childId)
         .attr('width', 100)
         .attr('height', 100)
//      new EditorView({
//         state: startState,
//         parent: div
//      })
   }
}

export var addEditorView = addEditorView_
