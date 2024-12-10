module Module where

import Prelude

import Control.Monad.Error.Class (liftEither)
import Control.Monad.Except (class MonadError)
import Data.Bifunctor (lmap)
import Data.Newtype (class Newtype)
import Data.List ((:))
import Effect.Aff.Class (class MonadAff)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import Lattice (Raw)
import Parse as P
import Parsing (runParser)
import ProgCxt (ProgCxt(..))
import SExpr (desugarModuleFwd)
import SExpr as S
import Util (AffError)
import Util.Parse (SParser)

type FileLoader = forall m. Folder -> File -> AffError m String

newtype File = File String
newtype Folder = Folder String

derive instance Newtype File _
derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. FileLoader -> Folder -> File -> AffError m (Raw S.Expr)
parseProgram loadFile folder file =
   loadFile folder file >>= flip parse P.program

module_ :: forall m. MonadAff m => MonadError Error m => FileLoader -> Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ loadFile folder file (ProgCxt r@{ mods }) = do
   src <- loadFile folder file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }
