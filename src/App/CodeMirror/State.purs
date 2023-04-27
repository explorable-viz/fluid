module App.CodeMirror.State where

import App.Util (HTMLId)
import Effect (Effect)

type EditorState = {
}

type EditorView = {
   state :: EditorState
}

foreign import addEditorView :: HTMLId -> Effect EditorView
