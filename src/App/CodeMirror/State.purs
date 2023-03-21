module App.CodeMirror.State where

import Web.HTML (HTMLElement)

type EditorView = {
   dom :: HTMLElement
}

foreign import view :: EditorView
