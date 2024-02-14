module Cardano.Types.PaymentPubKeyHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.FromData (class FromData)
import Cardano.ToData (class ToData)
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash(Ed25519KeyHash))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

-- not a CSL type
newtype PaymentPubKeyHash = PaymentPubKeyHash Ed25519KeyHash

derive instance Generic PaymentPubKeyHash _
derive instance Newtype PaymentPubKeyHash _
derive newtype instance Eq PaymentPubKeyHash
derive newtype instance FromData PaymentPubKeyHash
derive newtype instance Ord PaymentPubKeyHash
derive newtype instance ToData PaymentPubKeyHash
derive newtype instance EncodeAeson PaymentPubKeyHash
derive newtype instance DecodeAeson PaymentPubKeyHash

instance Show PaymentPubKeyHash where
  show = genericShow
