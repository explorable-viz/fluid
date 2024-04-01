module Util.Parse where

import Prelude hiding (absurd)

import Control.Alt ((<|>))
import Data.List (List(..))
import Data.List (many, some) as L
import Data.List.NonEmpty (NonEmptyList, cons', toList)
import Parsing (Parser)
import Parsing.Combinators (try)
import Util (nonEmpty)

type SParser = Parser String

-- helpers (could generalise further)
sepBy_try :: forall a sep. SParser a -> SParser sep -> SParser (List a)
sepBy_try p sep = (sepBy1_try p sep <#> toList) <|> pure Nil

sepBy1_try :: forall a sep. SParser a -> SParser sep -> SParser (NonEmptyList a)
sepBy1_try p sep = cons' <$> p <*> L.many (try $ sep *> p)

some :: forall a. SParser a → SParser (NonEmptyList a)
some p = nonEmpty <$> L.some p
