{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = ""
, dependencies =
  [ "aff"
  , "affjax"
  , "affjax-web"
  , "assert"
  , "console"
  , "debug"
  , "effect"
  , "exceptions"
  , "exists"
  , "fork"
  , "lists"
  , "node-fs"
  , "numbers"
  , "parsing"
  , "pprint"
  , "profunctor"
  , "psci-support"
  , "result"
  , "spec"
  , "spec-mocha"
  , "strings"
  , "tuples"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
