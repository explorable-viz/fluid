module App.View.Paragraph where

import Prelude

import App.Util.Selector (SelSetter, constrArg, docElement, listElement)
import App.View.Util (class Drawable, View, Select, createRootElement, setSelStates, unpack)
import App.View.Util.D3 (create, ElementType(..))
import App.View.Util.D3 as D3
import Data.Array (mapWithIndex)
import Data.Foldable (sequence_)
import DataType (cParagraph)
import Effect (Effect)
import Val (Val)

data Paragraph = Paragraph Boolean (Array View)

instance Drawable Paragraph where
   createRootElement _ = createRootElement'
   setSelStates = setSelStates'

createRootElement' :: Paragraph -> D3.Selection -> Effect D3.Selection
createRootElement' (Paragraph _ views) parent = do
   rootElement <- parent # create G []
   sequence_ $ flip map views \view -> do
      unpack view \v -> createRootElement (const []) v rootElement
   pure rootElement

setSelStates' :: Paragraph -> Select -> D3.Selection -> Effect Unit
setSelStates' (Paragraph isDoc views) select rootElement = do
   sequence_ $ flip mapWithIndex views \i view -> do
      unpack view \v -> setSelStates v (select <<< lift i) rootElement
   where
   lift :: Int -> SelSetter Val Val
   lift i = if isDoc then docElement i else constrArg cParagraph 0 <<< listElement i

