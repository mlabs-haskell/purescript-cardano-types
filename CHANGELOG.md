# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# v4.1.0

- introduce `Cardano.Types.Chain` module with exported types: `Chain`, `ChainTip`, `BlockHeaderHash`
- introduce `Cardano.Types.DelegationsAndRewards` module with exported types: `DelegationsAndRewards`
- introduce `Cardano.Types.EraSummaries` module with exported types: `EpochLength`, `EraSummaries`, `EraSummary`, `EraSummaryParameters`, `EraSummaryTime`, `RelativeTime`, `SafeZone`, `SlotLength`

# v4.0.0

## Changed

- Synchronized with `@mlabs-haskell/cardano-serialization-lib-gc` version `v13.2.0`.
- `Transaction.hash` now internally converts a `Transaction` to `FixedTransaction` under the hood. Context: The previously used `hash_transaction` function has been removed from CSL as it could not guarantee the correctness of a transaction hash for transactions created by third-party tools.

# v3.0.0

## Changed

- Synchronized with `@mlabs-haskell/cardano-serialization-lib-gc` version `v12.0.0`.
- Renamed `DrepVotingThresholds` to `DRepVotingThresholds`. 

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
