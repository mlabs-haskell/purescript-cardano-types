{-
Welcome to your new Dhall package-set!

Below are instructions for how to edit this file for most use
cases, so that you don't need to know Dhall to use it.

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
where `entityName` is one of the following:
- dependencies
- repo
- version
-------------------------------
let upstream = --
in  upstream
  with packageName.entityName = "new value"
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with halogen.version = "master"
  with halogen.repo = "https://example.com/path/to/git/repo.git"

  with halogen-vdom.version = "v4.0.0"
  with halogen-vdom.dependencies = [ "extra-dependency" ] # halogen-vdom.dependencies
-------------------------------

### Additions

Purpose:
- Add packages that aren't already included in the default package set

Syntax:
where `<version>` is:
- a tag (i.e. "v4.0.0")
- a branch (i.e. "master")
- commit hash (i.e. "701f3e44aafb1a6459281714858fadf2c4c2a977")
-------------------------------
let upstream = --
in  upstream
  with new-package-name =
    { dependencies =
       [ "dependency1"
       , "dependency2"
       ]
    , repo =
       "https://example.com/path/to/git/repo.git"
    , version =
        "<version>"
    }
-------------------------------

Example:
-------------------------------
let upstream = --
in  upstream
  with benchotron =
      { dependencies =
          [ "arrays"
          , "exists"
          , "profunctor"
          , "strings"
          , "quickcheck"
          , "lcg"
          , "transformers"
          , "foldable-traversable"
          , "exceptions"
          , "node-fs"
          , "node-buffer"
          , "node-readline"
          , "datetime"
          , "now"
          ]
      , repo =
          "https://github.com/hdgarrood/purescript-benchotron.git"
      , version =
          "v7.0.0"
      }
-------------------------------
-}
let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.10-20231023/packages.dhall
        sha256:b9a482e743055ba8f2d65b08a88cd772b59c6e2084d0e5ad854025fa90417fd4

let additions =
      { aeson =
        { dependencies =
          [ "aff"
          , "argonaut"
          , "argonaut-codecs"
          , "argonaut-core"
          , "arrays"
          , "bifunctors"
          , "const"
          , "control"
          , "effect"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "js-bigints"
          , "lists"
          , "maybe"
          , "mote"
          , "numbers"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "record"
          , "spec"
          , "strings"
          , "tuples"
          , "typelevel"
          , "typelevel-prelude"
          , "uint"
          , "untagged-union"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-aeson.git"
        , version = "v2.0.0"
        }
      , properties =
        { dependencies = [ "prelude", "console" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-properties.git"
        , version = "v0.2.0"
        }
      , lattice =
        { dependencies = [ "prelude", "console", "properties" ]
        , repo = "https://github.com/Risto-Stevcev/purescript-lattice.git"
        , version = "v0.3.0"
        }
      , bytearrays =
        { dependencies =
          [ "aeson"
          , "aff"
          , "arraybuffer-types"
          , "effect"
          , "either"
          , "foldable-traversable"
          , "maybe"
          , "newtype"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "spec"
          , "strings"
          ]
        , repo = "https://github.com/mlabs-haskell/purescript-bytearrays"
        , version = "e3991d562a04d8825472551d91a06407ad9c9112"
        }
      , cardano-serialization-lib =
        { dependencies =
          [ "aeson"
          , "argonaut"
          , "bifunctors"
          , "bytearrays"
          , "effect"
          , "either"
          , "maybe"
          , "nullable"
          , "ordered-collections"
          , "prelude"
          , "profunctor"
          , "tuples"
          ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-serialization-lib"
        , version = "ff43326e48654f3f8653d0915636dcc8ab460fa7"
        }
      , cardano-plutus-data-schema =
        { dependencies = [ "prelude" ]
        , repo =
            "https://github.com/mlabs-haskell/purescript-cardano-plutus-data-schema"
        , version = "351003d931990949ce3d73f399a179e4a0fa40f1"
        }
      }

in  upstream // additions
