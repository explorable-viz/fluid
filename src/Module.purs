module Module where

import Prelude

import Affjax.ResponseFormat (string)
import Affjax.Web (defaultRequest, printError, request)
import Bindings (Var)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (bimap)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Traversable (traverse)
import Desugarable (desug)
import Dict (singleton) as D
import Effect.Aff (Aff)
import Eval (eval, eval_module)
import EvalGraph (eval, eval_module) as G
import Expr (traverseModule)
import Graph (class Graph, Vertex)
import Graph (empty) as G
import Graph.GraphWriter (WithGraphT, runWithGraphT, alloc, alloc')
import Lattice (𝔹, bot, botOf)
import Parse (module_, program)
import Parsing (runParser)
import Primitive.Defs (primitives)
import SExpr (Expr) as S
import SExpr (desugarModuleFwd)
import Set (class Set, empty)
import Util (MayFail, type (×), (×), error, successful, both)
import Util.Parse (SParser)
import Val (Env, (<+>))

-- Mainly serve as documentation
newtype File = File String
newtype Folder = Folder String

derive newtype instance Show File
derive newtype instance Semigroup File
derive newtype instance Monoid File

resourceServerUrl :: String
resourceServerUrl = "."

loadFile :: Folder -> File -> Aff String
loadFile (Folder folder) (File file) = do
   let url = resourceServerUrl <> "/" <> folder <> "/" <> file <> ".fld"
   result <- request (defaultRequest { url = url, method = Left GET, responseFormat = string })
   case result of
      Left err -> error (printError err)
      Right response -> pure response.body

parse :: forall t. String -> SParser t -> MayFail t
parse src = runParser src >>> show `bimap` identity

parseProgram :: Folder -> File -> Aff (S.Expr Unit)
parseProgram folder file = do
   src <- loadFile folder file
   pure (successful $ flip parse (program <#> botOf) src)

open :: File -> Aff (S.Expr Unit)
open = parseProgram (Folder "fluid/example")

open𝔹 ∷ File -> Aff (S.Expr 𝔹)
open𝔹 = open >>= map botOf >>> pure

loadModule :: File -> Env Unit -> Aff (Env Unit)
loadModule file γ = do
   src <- loadFile (Folder "fluid/lib") file
   pure $ successful $
      (parse src (module_ <#> botOf) >>= desugarModuleFwd >>= flip (eval_module γ) bot) <#> (γ <+> _)

loadModuleG :: forall s. Set s Vertex => File -> Env Vertex -> WithGraphT s Aff (Env Vertex)
loadModuleG file γα = do
   src <- lift $ lift $ loadFile (Folder "fluid/lib") file
   modα <- traverseModule alloc' (successful $ parse src (module_) >>= desugarModuleFwd)
   G.eval_module γα modα empty <#> (γα <+> _)

defaultImports :: Aff (Env Unit)
defaultImports =
   loadModule (File "prelude") (primitives <#> botOf) >>= loadModule (File "graphics") >>= loadModule (File "convolution")

defaultImportsG :: forall s. Set s Vertex => WithGraphT s Aff (Env Vertex)
defaultImportsG = do
   γα <- traverse alloc primitives
   loadModuleG (File "prelude") γα >>= loadModuleG (File "graphics") >>= loadModuleG (File "convolution")

openWithDefaultImports :: File -> Aff (Env Unit × S.Expr Unit)
openWithDefaultImports file = do
   γ <- defaultImports
   open file <#> (γ × _)

openWithDefaultImports𝔹 :: File -> Aff (Env 𝔹 × S.Expr 𝔹)
openWithDefaultImports𝔹 file = do
   γ × s <- openWithDefaultImports file
   pure ((botOf <$> γ) × (botOf s))

openWithDefaultImportsG :: forall g s. Graph g s => File -> Aff ((g × Int) × Env Vertex × S.Expr Unit)
openWithDefaultImportsG file = successful <$>
   ( runWithGraphT (G.empty × 0) $ do
        γ <- defaultImportsG
        lift $ lift $ open file <#> (γ × _)
   )

-- Return ambient environment used to load dataset along with new binding.
openDatasetAs :: File -> Var -> Aff (Env Unit × Env Unit)
openDatasetAs file x = do
   s <- parseProgram (Folder "fluid") file
   γ <- defaultImports
   let _ × v = successful (desug (botOf s) >>= flip (eval γ) bot)
   pure (γ × D.singleton x v)

-- -- Return ambient environment used to load dataset along with new binding.
openDatasetAs𝔹 :: File -> Var -> Aff (Env 𝔹 × Env 𝔹)
openDatasetAs𝔹 file x = openDatasetAs file x >>= both (map botOf) >>> pure

openDatasetAsG :: forall g s. Graph g s => File -> Var -> Aff ((g × Int) × (Env Vertex × Env Vertex))
openDatasetAsG file x = do
   s <- parseProgram (Folder "fluid") file
   successful <$>
      ( runWithGraphT (G.empty × 0) $ do
           γα <- defaultImportsG
           eα <- alloc (successful $ desug s)
           vα <- G.eval γα eα empty
           pure (γα × D.singleton x vα)
      )