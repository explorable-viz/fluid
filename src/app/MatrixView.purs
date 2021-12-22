module App.MatrixView where

import Prelude hiding (absurd)
import Data.Array (zip, zipWith)
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (target)
import Web.Event.EventTarget (EventTarget)
import App.Util (Handler, Renderer)
import Lattice (Slice, 𝔹, expand, neg)
import Primitive (match_fwd)
import Util (type (×), (×), (!), absurd, error, fromJust)
import Val (Val(..), Array2, MatrixRep, holeMatrix, insertMatrix)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: Renderer MatrixView

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j

matrixViewHandler :: Handler
matrixViewHandler ev (u × Matrix _ (_ × (i' × _) × (j' × _))) = 
   case expand u (Matrix false (holeMatrix i' j')) of
      Matrix α (vss × (_ × β) × (_ × β')) ->
         Matrix α (insertMatrix i j (neg vss!(i - 1)!(j - 1)) (vss × (i' × β) × (j' × β')))
      _ -> error absurd
   where
      -- (unsafe) datum associated with matrix view mouse event; indices of selected cell
      unsafePos :: Maybe EventTarget -> Int × Int
      unsafePos tgt_opt =
         let tgt = fromJust absurd $ tgt_opt 
             xy = (unsafeCoerce tgt).__data__!0 :: Array Int 
         in xy!0 × xy!1

      i × j = unsafePos $ target ev
matrixViewHandler _ _ = error absurd
