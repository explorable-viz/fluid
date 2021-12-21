module App.MatrixView where

import Prelude hiding (absurd)
import Data.Array (zip, zipWith)
import Data.Maybe (Maybe)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, Renderer)
import Lattice (𝔹)
import Primitive (Slice, match_fwd)
import Util (type (×), (×), (!), absurd, error, fromJust)
import Test.Util (selectCell)
import Val (Val(..), Array2, MatrixRep)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j

matrixViewHandler :: Handler
matrixViewHandler redraw ev = do
   log $ "Selecting cell " <> show i <> ", " <> show j
   redraw selectCell'
   where
      -- (unsafe) the datum associated with a matrix view mouse event.
      unsafePos :: Maybe EventTarget -> Int × Int
      unsafePos tgt_opt =
         let tgt = fromJust absurd $ tgt_opt 
             xy = (unsafeCoerce tgt).__data__!0 :: Array Int 
         in xy!0 × xy!1

      i × j = unsafePos $ target ev

      selectCell' :: Slice (Val 𝔹) -> Val 𝔹
      selectCell' (_ × Matrix _ (_ × (h × _) × (w × _))) = selectCell i j h w
      selectCell' _ = error absurd
