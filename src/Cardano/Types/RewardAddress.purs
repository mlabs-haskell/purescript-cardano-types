module Cardano.Types.RewardAddress where

import Prelude

import Cardano.Data.Lite
  ( address_fromBech32
  , address_networkId
  , address_toBech32
  , rewardAddress_fromAddress
  , rewardAddress_new
  , rewardAddress_paymentCred
  , rewardAddress_toAddress
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Credential as Credential
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.StakeCredential (StakeCredential)
import Data.Int as Int
import Data.Maybe (Maybe, fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Literals.Undefined (undefined)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type RewardAddress =
  { networkId :: NetworkId
  , stakeCredential :: StakeCredential
  }

toBech32 :: RewardAddress -> Bech32String
toBech32 =
  toCsl >>> rewardAddress_toAddress >>> flip address_toBech32 (unsafeCoerce undefined)

fromBech32 :: Bech32String -> Maybe RewardAddress
fromBech32 =
  map fromCsl <<< toMaybe <<< rewardAddress_fromAddress <=<
    toMaybe <<< address_fromBech32

toCsl :: RewardAddress -> Csl.RewardAddress
toCsl { networkId, stakeCredential } =
  rewardAddress_new
    (Int.toNumber $ NetworkId.toInt networkId)
    (Credential.toCsl $ unwrap stakeCredential)

fromCsl :: Csl.RewardAddress -> RewardAddress
fromCsl addr =
  { networkId
  , stakeCredential: wrap $ Credential.fromCsl $ rewardAddress_paymentCred addr
  }
  where
  networkId :: NetworkId
  networkId =
    unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust
      $ Int.fromNumber
      $ address_networkId
      $ rewardAddress_toAddress addr
