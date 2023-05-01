"use strict"

import * as d3 from "d3"

// This prelude currently duplicated across all FFI implementations.
function curry2 (f) {
   return x1 => x2 => f(x1, x2)
}

function curry3 (f) {
   return x1 => x2 => x3 => f(x1, x2, x3)
}

function curry4 (f) {
   return x1 => x2 => x3 => x4 => f(x1, x2, x3, x4)
}

import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"

let startState = EditorState.create({
  doc: "Hello World",
  extensions: [keymap.of(defaultKeymap)]
})

function addEditorView_ (id) {
   return () => {
      const div = d3.select('#' + id).node()
      return new EditorView({
         state: startState,
         parent: div
      })
   }
}

function replaceSelection_ (editorState, str) {
   return editorState.replaceSelection(str)
}

function dispatch_ (editorView, specs) {
   return () => {
      trans = editorView.state.update(specs)
      console.log(trans.state.doc.toString())
      editorView.dispatch(trans)
   }
}

function blah_(editorView, str) {
   return () => {
      const trans = editorView.state.update({changes: {from: 1, to: 3, insert: str}})
      console.log(trans.state.doc.toString())
      editorView.dispatch(trans)
   }
}

export var addEditorView = addEditorView_
export var blah = curry2(blah_)
export var dispatch = curry2(dispatch_)
export var replaceSelection = curry2(replaceSelection_)
