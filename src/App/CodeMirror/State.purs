module App.CodeMirror.State where

import Prelude

import App.Util (HTMLId)
import Effect (Effect)

foreign import addEditorView :: HTMLId -> Effect Unit
