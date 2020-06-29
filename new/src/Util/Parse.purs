module Util.Parse where

import Prelude hiding (absurd)
import Control.Alt ((<|>))
import Control.Apply (lift2)
import Data.List (List(..), (:), many)
import Data.List (some) as L
import Data.List.NonEmpty (NonEmptyList, fromList)
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Combinators (sepBy1) as P
import Util (absurd, fromJust)

type SParser = Parser String

-- helpers (could generalise further)
sepBy1 :: forall a sep . SParser a -> SParser sep -> SParser (NonEmptyList a)
sepBy1 p sep = fromJust absurd <$> (fromList <$> P.sepBy1 p sep)

sepBy_try :: forall a sep . SParser a -> SParser sep -> SParser (List a)
sepBy_try p sep = sepBy1_try p sep <|> pure Nil

sepBy1_try :: forall a sep . SParser a -> SParser sep -> SParser (List a)
sepBy1_try p sep = p `lift2 (:)` many (try $ sep *> p)

some :: forall a . SParser a → SParser (NonEmptyList a)
some p = fromJust absurd <$> (fromList <$> L.some p)
