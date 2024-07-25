# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# v2.0.1

## Added

More utils for end users:

- `Cardano.Types.Transaction`: `findUtxos`
- `Cardano.Types.TransactionInput`: `pprintTransactionInput`
- `Cardano.Types.TransactionUnspentOutput`: `filterUtxos`, `hasTransactionHash`

## Fixed

- Do not add empty Withdrawals map to serialized TransactionBody ([#11](https://github.com/mlabs-haskell/purescript-cardano-types/pull/11))

# v2.0.0

## Changed

- Moved to `Conway` era and `@mlabs-haskell/cardano-serialization-lib-gc` version `v12.0.0-alpha.31`.
- `datum` field of a `Redeemer`: use `RedeemerDatum` instead of `PlutusData`

## Added

- `RedeemerDatum` type (a tagged newtype over `PlutusData`)
- Field access lenses for main domain types (`Transaction`, `TransactionOutput`, `TransactionInput`)

# v1.0.1

## Changed

- `Aeson` instances for `AssetName`: encode as byte string instead of Plutus-compatible encoding. The instances have been moved to `purescript-plutus-types` (`TokenName`).

# v1.0.0

First complete version of the library
