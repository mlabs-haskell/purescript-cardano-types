{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "cardano-types"
, dependencies =
  [ "aeson"
  , "aff"
  , "arraybuffer-types"
  , "arrays"
  , "bifunctors"
  , "bytearrays"
  , "cardano-plutus-data-schema"
  , "cardano-serialization-lib"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "encoding"
  , "exceptions"
  , "foldable-traversable"
  , "foreign-object"
  , "integers"
  , "js-bigints"
  , "lattice"
  , "lists"
  , "literals"
  , "maybe"
  , "monad-logger"
  , "mote"
  , "mote-testplan"
  , "newtype"
  , "nonempty"
  , "nullable"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "profunctor"
  , "profunctor-lenses"
  , "quickcheck"
  , "rationals"
  , "record"
  , "safe-coerce"
  , "spec"
  , "these"
  , "tuples"
  , "typelevel-prelude"
  , "uint"
  , "unfoldable"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
