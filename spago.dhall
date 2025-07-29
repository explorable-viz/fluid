{-
Welcome to a Spago project!
You can edit this file as you like.
-}
let baseSources =
      [ "src/**/*.purs"
      , "test/**/*.purs"
      , "website/**/*.purs"
      , "config/*.purs"
      ]

in  { name = ""
    , dependencies =
      [ "aff"
      , "aff-promise"
      , "affjax"
      , "affjax-web"
      , "argonaut-codecs"
      , "argonaut-core"
      , "arrays"
      , "bifunctors"
      , "console"
      , "control"
      , "debug"
      , "effect"
      , "either"
      , "exceptions"
      , "filterable"
      , "foldable-traversable"
      , "foreign"
      , "foreign-object"
      , "functions"
      , "functors"
      , "graphs"
      , "http-methods"
      , "identity"
      , "integers"
      , "lists"
      , "maybe"
      , "newtype"
      , "node-buffer"
      , "node-fs"
      , "nonempty"
      , "numbers"
      , "optparse"
      , "ordered-collections"
      , "parsing"
      , "partial"
      , "prelude"
      , "profunctor"
      , "st"
      , "strings"
      , "tailrec"
      , "toppokki"
      , "transformers"
      , "tuples"
      , "unfoldable"
      , "unicode"
      , "unsafe-coerce"
      , "web-events"
      ]
    , packages = ./packages.dhall
    , baseSources
    , sources = baseSources # [ "config/dev/**/*.purs" ]
    , backend = "purs-backend-es build"
    }
