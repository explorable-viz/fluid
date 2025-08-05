module Module where

import Prelude

import Bind (Bind, (↦))
import Control.Monad.Error.Class (liftEither, throwError)
import Control.Monad.Except (class MonadError)
import Control.Monad.Reader (class MonadReader, ask)
import Data.Argonaut.Core (Json, caseJson, toNumber)
import Data.Argonaut.Decode (parseJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Profunctor.Strong (second)
import Desugarable (desug)
import Doc (DocOpt(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Effect.Exception (Error)
import Effect.Exception (error) as E
import EvalGraph (GraphConfig, eval_progCxt)
import Expr (class FV, Expr, fv)
import File (class LoadFile, File(..), FileCxt(..), Folder, loadFile)
import Graph (Vertex, vertices)
import Graph.GraphImpl (GraphImpl)
import Graph.WithGraph (class MonadAlloc, AllocT, alloc, alloc_check, runAllocT, runWithGraphT_spy)
import Lattice (Raw)
import Parse as P
import Parsing (runParser)
import Primitive.Defs (primitives)
import ProgCxt (ProgCxt(..))
import SExpr (desugarModuleFwd)
import SExpr as S
import Test.Util.Debug (checking)
import Util (type (×), AffError, concatM, debug, error, spy, (×))
import Util.Map (restrict)
import Util.Parse (SParser)
import Val (Val(..))
import Val as V

-- import Data.Array (toUnfoldable) as Array
-- import Foreign.Object as Object

parse :: forall a m. MonadError Error m => String -> SParser a -> m a
parse src = liftEither <<< lmap (E.error <<< show) <<< runParser src

parseProgram :: forall m. LoadFile m => Array Folder -> File -> AffError m (Raw S.Expr)
parseProgram folders file =
   loadFile folders file >>= flip parse P.program

module_ :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> File -> Raw ProgCxt -> m (Raw ProgCxt)
module_ folders file (ProgCxt r@{ mods }) = do
   when debug.logging $ log ("module_: " <> show (folders × file))
   src <- loadFile folders file
   mod <- parse src P.module_ >>= desugarModuleFwd
   pure $ ProgCxt r { mods = mod : mods }

datasetAs :: forall m. MonadAff m => MonadError Error m => LoadFile m => Array Folder -> Bind File -> Raw ProgCxt -> m (Raw ProgCxt)
datasetAs folders (x ↦ file) (ProgCxt r@{ datasets }) = do
   eα <- parseProgram folders file >>= desug
   pure $ ProgCxt r { datasets = (x ↦ eα) : datasets }

loadJson :: forall m. MonadAff m => MonadAlloc m => MonadError Error m => LoadFile m => MonadReader FileCxt m => String -> m (Val Vertex)
loadJson path = do
   FileCxt { fluidSrcPaths } <- ask
   log $ "Loading JSON file: " <> path
   log $ "fluidSrcPaths: " <> show fluidSrcPaths
   -- loaadFile currently uses .fld. Idea: have loadJson remove the automatic .fld extension
   -- and just load the file as is, so we can use .json files directly.
   -- For now, we just load the .fld file.
   jfile <- loadFile fluidSrcPaths (File path)
   log $ "File contents: " <> jfile
   case parseJson jfile of
      Left err -> throwError $ error ("Failed to parse JSON: " <> show err)
      Right j -> do
         alloc (fromJsonVal j)

-- use casejson
-- make a recursive check with the layout: array,object,string,number
-- after checks for array and object we can then assume it is only string and numbers left
-- This function converts a Json value to a Val Unit.
-- use the spy indentity to debug the value
-- recurse over the JSON structure

fromJsonVal :: Json -> Val Unit
-- fromJsonVal j = do
--    -- case toString j of
--    --    Just s -> spy "json value" identity (Val unit None (V.Str s))
--    --    Nothing -> error ("FromJsonVal not implemented yet")

--    case toNumber j of
--       Just n -> spy "json value" identity (Val unit None (V.Float n))
--       Nothing -> error ("FromJsonVal not implemented yet")
-- --       case toArray j of
-- --          Just arr -> spy "json value" identity (Val unit None (V.Constr arr))
-- --          Nothing -> error ("FromJsonVal not implemented yet")
-- --             case toObject j of
-- --                Just obj -> spy "json value" identity (Val unit None (V.Dictionary (DictRep obj)))
-- --                Nothing -> error ("FromJsonVal not implemented yet")

fromJsonVal json = caseJson
  (\_ -> spy "Processing null" identity (Val unit None (V.Str "Null")))
  (\b -> spy "Processing boolean" identity (Val unit None (V.Str (show b))))
  (\n -> spy "Processing number" identity (Val unit None (V.Float n)))
  (\s -> spy "Processing string" identity (Val unit None (V.Str s)))
  (\arr ->
       let vals = Array.toUnfoldable (map fromJsonVal arr) :: List (Val Unit)
       in arrVtoVal vals
   --  let vals = Array.toUnfoldable (map fromJsonVal arr)
   --  in Val unit None (V.Constr "Array" vals)
  )
  (\obj -> ?_
   --  let
   --    pairs = map (\(Tuple k v) -> Tuple k (fromJsonVal v)) (Object.toUnfoldable obj)
   --    dict = Object.fromFoldable pairs
   --  in Val unit None (V.Dictionary (DictRep dict))
  )
  json

arrVtoVal :: List (Val Unit) -> Val Unit
-- arrVtoVal Nil = Val unit None (V.Constr "Nil" Nil)
-- arrVtoVal (x : xs) = Val unit None (Constr "Cons" [x, arrVtoVal xs])

arrVtoVal vs = error "arrVtoVal not implemented yet"


loadProgCxt :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => Array String -> Array (Bind String) -> m (Raw ProgCxt)
loadProgCxt mods datasets = do
   FileCxt { fluidSrcPaths } <- ask
   pure (ProgCxt { primitives, mods: Nil, datasets: Nil })
      >>= concatM (File >>> module_ fluidSrcPaths <$> [ "lib/prelude" ] <> mods)
      >>= concatM (second File >>> datasetAs fluidSrcPaths <$> datasets)

initialConfig :: forall m a. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => FV a => a -> Raw ProgCxt -> m GraphConfig
initialConfig e progCxt = do
   when checking.allocRoundTrip $ alloc_check "progCxt" (alloc progCxt)
   n × _ × progCxt' × γ <- flip runAllocT 0 do
      progCxt' <- alloc progCxt
      let αs = vertices progCxt'
      _ × γ <- runWithGraphT_spy (eval_progCxt progCxt') αs :: AllocT m (GraphImpl × _)
      pure (progCxt' × restrict (fv e) γ)
   pure { n, progCxt: progCxt', γ }

type Config = { s :: Raw S.Expr, e :: Raw Expr, gconfig :: GraphConfig }

prepConfig :: forall m. MonadAff m => MonadError Error m => MonadReader FileCxt m => LoadFile m => File -> Raw ProgCxt -> m Config
prepConfig file progCxt = do
   FileCxt { fluidSrcPaths } <- ask
   s <- parseProgram fluidSrcPaths file
   e <- desug s
   gconfig <- initialConfig e progCxt
   pure { s, e, gconfig }
