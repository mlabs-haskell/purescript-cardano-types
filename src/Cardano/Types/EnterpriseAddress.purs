module Cardano.Types.EnterpriseAddress where

import Prelude

import Cardano.Data.Lite
  ( address_networkId
  , enterpriseAddress_new
  , enterpriseAddress_paymentCred
  , enterpriseAddress_toAddress
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.Credential as Credential
import Cardano.Types.NetworkId (NetworkId)
import Cardano.Types.NetworkId as NetworkId
import Cardano.Types.PaymentCredential (PaymentCredential)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (unwrap, wrap)
import Partial.Unsafe (unsafePartial)

type EnterpriseAddress =
  { networkId :: NetworkId
  , paymentCredential :: PaymentCredential
  }

toCsl :: EnterpriseAddress -> Csl.EnterpriseAddress
toCsl = case _ of
  { networkId, paymentCredential } ->
    enterpriseAddress_new (Int.toNumber $ NetworkId.toInt networkId) (Credential.toCsl $ unwrap paymentCredential)

fromCsl :: Csl.EnterpriseAddress -> EnterpriseAddress
fromCsl addr =
  { networkId
  , paymentCredential: wrap $ Credential.fromCsl $ enterpriseAddress_paymentCred addr
  }
  where
  networkId :: NetworkId
  networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust $ Int.fromNumber $ address_networkId $
    enterpriseAddress_toAddress addr
