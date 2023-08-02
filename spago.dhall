{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = ""
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "arrays"
  , "benchotron"
  , "bifunctors"
  , "console"
  , "control"
  , "datetime"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "exists"
  , "foldable-traversable"
  , "foreign-object"
  , "formatters"
  , "gen"
  , "http-methods"
  , "identity"
  , "integers"
  , "lcg"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-readline"
  , "nonempty"
  , "now"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "prelude"
  , "profunctor"
  , "quickcheck"
  , "spec"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unicode"
  , "unsafe-coerce"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, backend = "purs-backend-es build"
}
