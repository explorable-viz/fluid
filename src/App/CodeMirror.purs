module App.CodeMirror where

import Prelude
import App.View.Util (HTMLId)
import Effect (Effect)

-- The CodeMirror API, documented in TypeScript, is horrendous. Expose the smallest subset that
-- supports our use cases.

type ChangeSpec =
   { from :: Int
   , to :: Int
   , insert :: String
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
foreign import dispatch :: EditorView -> Transaction -> Effect Unit
foreign import getContentsLength :: EditorView -> Int
foreign import replaceSelection :: EditorState -> String -> TransactionSpec
foreign import update :: EditorState -> Array TransactionSpec -> Effect Transaction
