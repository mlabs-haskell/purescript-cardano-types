module Cardano.Types.PaymentCredential where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(TypeMismatch), caseAesonString, encodeAeson)
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Types.Credential (Credential)
import Data.ByteArray (hexToByteArray)
import Data.Either (Either(Left, Right))
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

-- in CSL, StakeCredential and PaymentCredential are the same type, because they are
-- literally the same, but we treat them differently as domain types
newtype PaymentCredential = PaymentCredential Credential

derive instance Newtype PaymentCredential _
derive instance Generic PaymentCredential _

instance Arbitrary PaymentCredential where
  arbitrary = genericArbitrary

instance AsCbor PaymentCredential where
  encodeCbor = unwrap >>> encodeCbor
  decodeCbor = decodeCbor >>> map wrap

instance Eq PaymentCredential where
  eq = eq `on` encodeCbor

instance Ord PaymentCredential where
  compare = compare `on` encodeCbor

instance Show PaymentCredential where
  show = genericShow

-- Corresponds to Plutus' `Plutus.V1.Ledger.Api.Script` Aeson instances
instance DecodeAeson PaymentCredential where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded PaymentCredential") Right <<<
      caseAesonString Nothing (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson PaymentCredential where
  encodeAeson sh = encodeAeson $ encodeCbor sh
