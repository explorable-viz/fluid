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
  , "lists"
  , "node-fs"
  , "parsing"
  , "psci-support"
  , "result"
  , "spec"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
