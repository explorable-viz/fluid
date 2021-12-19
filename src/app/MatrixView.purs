module App.MatrixView where

import Prelude hiding (absurd)
import Data.Array (zip, zipWith)
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Console (log)
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (Event, target)
import Web.Event.EventTarget (EventListener, EventTarget)
import App.Util (HTMLId)
import Lattice (𝔹)
import Primitive (Slice, match_fwd)
import Util (type (×), (×), (!), absurd, fromJust)
import Val (Array2, MatrixRep)

--  (Rendered) matrices are required to have element type Int for now.
type IntMatrix = Array2 (Int × 𝔹) × Int × Int
newtype MatrixView = MatrixView { title :: String, matrix :: IntMatrix }

foreign import drawMatrix :: HTMLId -> MatrixView -> EventListener -> Effect Unit

matrixRep :: Slice (MatrixRep 𝔹) -> IntMatrix
matrixRep ((vss × _ × _) × (uss × (i × _) × (j × _))) =
   ((<$>) ((<$>) match_fwd)) (zipWith zip vss uss) × i × j

matrixViewHandler :: (Unit -> Effect Unit) -> Event -> Effect Unit
matrixViewHandler redraw ev = do
   log $ show $ unsafePos $ target ev
   redraw unit

-- (unsafe) the datum associated with a matrix view mouse event.
unsafePos :: Maybe EventTarget -> Int × Int
unsafePos tgt_opt =
   let tgt = fromJust absurd $ tgt_opt in 
   let xy = (unsafeCoerce tgt).__data__!0 :: Array Int 
   in xy!0 × xy!1
