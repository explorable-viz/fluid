module App.CodeMirror where

import Prelude
import App.Util (HTMLId)
import Effect (Effect)

-- The CodeMirror API, documented in TypeScript, is horrendous. Expose the smallest subset that
-- supports our use cases.

type ChangeSpec =
   { from :: Number,
     to :: Number,
     insert :: String
   }

type EditorState =
   {
   }

type EditorView =
   { state :: EditorState
   }

type Transaction =
   {
   }

type TransactionSpec =
   { changes :: ChangeSpec
   }

foreign import addEditorView :: HTMLId -> Effect EditorView
foreign import dispatch :: EditorView -> Array TransactionSpec -> Effect Unit
foreign import blah :: EditorView -> String -> Effect Unit
foreign import replaceSelection :: EditorState -> String -> TransactionSpec
foreign import update :: EditorState -> Array TransactionSpec -> Transaction
