let base = ./spago.dhall
in base // {
    sources = base.baseSources # ["config/prod/**/*.purs"]
}
