# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

# v5.0.0

## Changed

- Replaced cardano-serialization-lib (CSL) with [cardano-data-lite (CDL)](https://github.com/mlabs-haskell/purescript-cardano-data-lite) ([#21](https://github.com/mlabs-haskell/purescript-cardano-types/pull/21))
  - `fromCsl` and `toCsl` functions renamed to `fromCdl` and `toCdl` respectively
- `PlutusScript.fromCdl` (previously `fromCsl`) now accepts an additional `Language` parameter, since the corresponding CDL type doesn't preserve the language tag ([#21](https://github.com/mlabs-haskell/purescript-cardano-types/pull/21))

## Removed

- `AsCbor` instance for `PlutusScript`. Please use the `PlutusScript.encodeCbor` and `PlutusScript.decodeCbor` functions instead ([#21](https://github.com/mlabs-haskell/purescript-cardano-types/pull/21))
- `PointerAddress` and the corresponding `Address` constructor [(marked as deprecated in Conway)](https://github.com/cardano-foundation/CIPs/blob/1b3913da832566ad2a46955fc91c859dc9e88537/CIP-0019/CIP-0019-cardano-addresses.abnf#L5-L6) ([#21](https://github.com/mlabs-haskell/purescript-cardano-types/pull/21))

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
