module Cardano.Types.EnterpriseAddress where

import Prelude

import Cardano.Data.Lite
  ( address_networkId
  , enterpriseAddress_new
  , enterpriseAddress_paymentCred
  , enterpriseAddress_toAddress
  )
import Cardano.Data.Lite as Cdl
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

toCdl :: EnterpriseAddress -> Cdl.EnterpriseAddress
toCdl = case _ of
  { networkId, paymentCredential } ->
    enterpriseAddress_new (Int.toNumber $ NetworkId.toInt networkId) (Credential.toCdl $ unwrap paymentCredential)

fromCdl :: Cdl.EnterpriseAddress -> EnterpriseAddress
fromCdl addr =
  { networkId
  , paymentCredential: wrap $ Credential.fromCdl $ enterpriseAddress_paymentCred addr
  }
  where
  networkId :: NetworkId
  networkId = unsafePartial $ fromJust $ NetworkId.fromInt $ fromJust $ Int.fromNumber $ address_networkId $
    enterpriseAddress_toAddress addr
