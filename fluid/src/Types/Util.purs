module Types.Util where

import Prim hiding (Type)

import Bind (Name, Var, qual)
import Data.Foldable (class Foldable)
import Data.List (List(..), fromFoldable, (:))
import ModuleGraph (builtins, prelude, view)
import Types (Primitive(..), Type(..))

object :: Type
object = PrimitiveTy Object

never :: Type
never = PrimitiveTy Never

none :: Type
none = PrimitiveTy None

bool :: Type
bool = PrimitiveTy Bool

int :: Type
int = PrimitiveTy Int

float :: Type
float = PrimitiveTy Float

str :: Type
str = PrimitiveTy Str

intPair :: Type
intPair = TupleTy (int : int : Nil)

callable :: forall f. Foldable f => f Type -> Type -> Type
callable σs τ = CallableTy (fromFoldable σs) τ

var :: Var -> Type
var = VarTy

matrix :: Type -> Type
matrix τ = ClassTy cMatrix (τ : Nil)

-- Used internally by primitives, desugaring or rendering layer.
cDefault = qual view "Default" :: Name -- Orientation
cRotated = qual view "Rotated" :: Name
cBarChart = qual view "BarChart" :: Name -- View
cLineChart = qual view "LineChart" :: Name
cLinePlot = qual view "LinePlot" :: Name
cMultiView = qual view "MultiView" :: Name
cScatterPlot = qual view "ScatterPlot" :: Name
cParagraph = qual view "Paragraph" :: Name
cDimensions = qual view "Dimensions" :: Name
cPoint = qual view "Point" :: Name
cAxisLabels = qual view "AxisLabels" :: Name
cTickLabels = qual view "TickLabels" :: Name
cSegment = qual view "Segment" :: Name
cStackedBar = qual view "StackedBar" :: Name
cNonEmpty = qual prelude "NonEmpty" :: Name -- Tree
cRange = qual builtins "range" :: Name
cMatrix = qual builtins "Matrix" :: Name
cText = qual view "Text" :: Name
cLink = qual view "Link" :: Name
