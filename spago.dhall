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
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "http-methods"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "nonempty"
  , "numbers"
  , "ordered-collections"
  , "parsing"
  , "partial"
  , "pprint"
  , "prelude"
  , "profunctor"
  , "spec"
  , "strings"
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
