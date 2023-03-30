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
      const div2 = document.createElement('div');
      new EditorView({
         state: startState,
         parent: div2
      })
   }
}

export var addEditorView = addEditorView_
