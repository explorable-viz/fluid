"use strict"

import * as d3 from "d3";
import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"

let startState = EditorState.create({
  doc: "Hello World",
  extensions: [keymap.of(defaultKeymap)]
})

function addEditorView_ (id) {
   const view = new EditorView({
      state: startState,
      parent: document.body
   })
   const div = d3.select('#' + id)
   div.append(view.dom)
}

export var addEditorView = addEditorView_
