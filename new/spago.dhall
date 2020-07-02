{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "affjax"
  , "assert"
  , "console"
  , "debug"
  , "effect"
  , "exceptions"
  , "exists"
  , "fork"
  , "lists"
  , "node-fs"
  , "parsing"
  , "pprint"
  , "profunctor"
  , "psci-support"
  , "result"
  , "spec"
  , "spec-mocha"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}