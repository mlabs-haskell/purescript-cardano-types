module Cardano.Types.PublicKey where

import Prelude

import Data.Newtype (class Newtype, unwrap, wrap)
import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Serialization.Lib (publicKey_asBytes, publicKey_fromBytes, publicKey_toBech32)
import Cardano.Serialization.Lib as Csl
import Cardano.FromData (class FromData, fromData)
import Cardano.Types.Internal.Helpers (eqOrd)
import Cardano.ToData (class ToData, toData)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Nullable (toMaybe)

newtype PublicKey = PublicKey Csl.PublicKey

derive instance Generic PublicKey _
derive instance Newtype PublicKey _

instance Eq PublicKey where
  eq = eqOrd

instance Ord PublicKey where
  compare = compare `on` (unwrap >>> publicKey_asBytes)

instance EncodeAeson PublicKey where
  encodeAeson = unwrap >>> publicKey_asBytes >>> encodeAeson

instance ToData PublicKey where
  toData (PublicKey pk) = toData $ publicKey_asBytes pk

instance FromData PublicKey where
  fromData = map wrap <<< toMaybe <<< publicKey_fromBytes <=< fromData

instance Show PublicKey where
  show pk = "(PublicKey " <> (publicKey_toBech32 <<< unwrap $ pk) <> ")"
