module App.CodeMirror.State where

import Prelude
import App.Util (HTMLId)
import Effect (Effect)

type TransactionSpec = {
}

type EditorState = {
}

type EditorView = {
   state :: EditorState
}

foreign import addEditorView :: HTMLId -> Effect EditorView
foreign import replaceSelection :: EditorState -> String -> TransactionSpec
foreign import dispatch :: EditorView -> Array TransactionSpec -> Effect Unit
