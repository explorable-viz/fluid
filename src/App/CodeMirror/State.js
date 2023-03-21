"use strict"

import {EditorState} from "@codemirror/state"
import {EditorView, keymap} from "@codemirror/view"
import {defaultKeymap} from "@codemirror/commands"

let startState = EditorState.create({
  doc: "Hello World",
  extensions: [keymap.of(defaultKeymap)]
})

function addEditorView_ (id) {
   return () => {
      const div = document.getElementById(id)
      new EditorView({
         state: startState,
         parent: div
      })
   }
}

export var addEditorView = addEditorView_
