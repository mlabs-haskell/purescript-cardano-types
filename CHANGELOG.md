# v1.0.2

## Added

- `RedeemerDatum` type (a tagged newtype over `PlutusData`)
- Field access lenses for main domain types (`Transaction`, `TransactionOutput`, `TransactionInput`)

## Changed

- `datum` field of a `Redeemer`: use `RedeemerDatum` instead of `PlutusData`

# v1.0.1

## Changed

- `Aeson` instances for `AssetName`: encode as byte string instead of Plutus-compatible encoding. The instances have been moved to `purescript-plutus-types` (`TokenName`).

# v1.0.0

First complete version of the library
