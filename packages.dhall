
{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

## Warning: Don't Move This Top-Level Comment!

Due to how `dhall format` currently works, this comment's
instructions cannot appear near corresponding sections below
because `dhall format` will delete the comment. However,
it will not delete a top-level comment like this one.

## Use Cases

Most will want to do one or both of these options:
1. Override/Patch a package's dependency
2. Add a package not already in the default package set

This file will continue to work whether you use one or both options.
Instructions for each option are explained below.

### Overriding/Patching a package

Purpose:
- Change a package's dependency to a newer/older release than the
    default package set's release
- Use your own modified version of some dependency that may
    include new API, changed API, removed API by
    using your custom git repo of the library rather than
    the package set's repo

Syntax:
Replace the overrides' "{=}" (an empty record) with the following idea
The "//" or "⫽" means "merge these two records and
  when they have the same value, use the one on the right:"
-------------------------------
let overrides =
  { packageName =
      upstream.packageName // { updateEntity1 = "new value", updateEntity2 = "new value" }
  , packageName =
      upstream.packageName // { version = "v4.0.0" }
  , packageName =
      upstream.packageName // { repo = "https://www.example.com/path/to/new/repo.git" }
  }
-------------------------------

Example:
-------------------------------
let overrides =
  { halogen =
      upstream.halogen // { version = "master" }
  , halogen-vdom =
      upstream.halogen-vdom // { version = "v4.0.0" }
  }
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
Replace the additions' "{=}" (an empty record) with the following idea:
-------------------------------
let additions =
  { package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , package-name =
       { dependencies =
           [ "dependency1"
           , "dependency2"
           ]
       , repo =
           "https://example.com/path/to/git/repo.git"
       , version =
           "tag ('v4.0.0') or branch ('master')"
       }
  , etc.
  }
-}

let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.4-20220808/packages.dhall sha256:60eee64b04ca0013fae3e02a69fc3b176105c6baa2f31865c67cd5f881a412fd

let overrides =
{ node-fs =
    upstream.node-fs // {version = "v9.1.0"}
}

let additions =
  { toppokki =
    { dependencies =
        [ "aff-promise"
        , "functions"
        , "node-buffer"
        , "node-fs"
        , "node-http"
        , "prelude"
        , "record"
        , "console"
        , "effect"
        , "assert"
        ]
    , repo =
      "https://github.com/justinwoo/purescript-toppokki.git"
    , version =
         "v4.0.0"
    }

  , js-promise-aff =
        { dependencies = [ "prelude" ]
        , repo = "https://github.com/purescript-contrib/purescript-js-promise-aff.git"
        , version = "v4.0.0"
        }
  }

in  upstream // overrides // additions
