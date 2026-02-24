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

import { EditorState } from "@codemirror/state"
import { EditorView, keymap, lineNumbers } from "@codemirror/view"
import { defaultKeymap } from "@codemirror/commands"
import { python } from "@codemirror/lang-python"
import { oneDark } from "@codemirror/theme-one-dark"

let startState = EditorState.create({
   doc: "",
   extensions:
      [ keymap.of(defaultKeymap)
      , EditorView.editable.of(false)
      , lineNumbers()
      , python()
      , oneDark
      ]
})

function getContentsLength_ (ed) {
   return ed.state.doc.length
}

function addEditorView_ (id) {
   return () => {
      const div = d3.select('#' + id).node()
      return new EditorView({
         state: startState,
         parent: div,
      })
   }
}

function replaceSelection_ (editorState, str) {
   return editorState.replaceSelection(str)
}

function dispatch_ (editorView, tr) {
   return () => {
      editorView.dispatch(tr)
   }
}

function update_(editorState, specs) {
   return () => {
      return editorState.update(...specs)
   }
}

export var addEditorView = addEditorView_
export var dispatch = curry2(dispatch_)
export var getContentsLength = getContentsLength_
export var replaceSelection = curry2(replaceSelection_)
export var update = curry2(update_)
