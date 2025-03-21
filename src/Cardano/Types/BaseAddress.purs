module Cardano.Types.BaseAddress where

import Prelude

import Cardano.Data.Lite
  ( address_networkId
  , baseAddress_new
  , baseAddress_paymentCred
  , baseAddress_stakeCred
  , baseAddress_toAddress
  )
import Cardano.Data.Lite as Cdl
import Cardano.Types.Credential as Credential
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
import Cardano.Types.StakeCredential (StakeCredential)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Partial.Unsafe (unsafePartial)

type BaseAddress =
  { networkId :: NetworkId
  , paymentCredential :: PaymentCredential
  , stakeCredential :: StakeCredential
  }

-- no AsCbor instance, because there is no to_bytes method in CSL

toCdl :: BaseAddress -> Cdl.BaseAddress
toCdl { networkId, paymentCredential, stakeCredential } =
  baseAddress_new
    (Int.toNumber $ NetworkId.toInt networkId)
    (Credential.toCdl $ unwrap paymentCredential)
    (Credential.toCdl $ unwrap stakeCredential)

fromCdl :: Cdl.BaseAddress -> BaseAddress
fromCdl addr =
  { networkId
  , paymentCredential: wrap $ Credential.fromCdl $ baseAddress_paymentCred addr
  , stakeCredential: wrap $ Credential.fromCdl $ baseAddress_stakeCred addr
  }
  where
  networkId :: NetworkId
  networkId =
    unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust
      $ Int.fromNumber
      $ address_networkId
      $ baseAddress_toAddress addr
