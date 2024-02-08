module Cardano.Types.PrivateKey where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (privateKey_asBytes, privateKey_toBech32, toBytes, fromBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (eqOrd)
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype PrivateKey = PrivateKey Csl.PrivateKey

derive instance Newtype PrivateKey _

instance Eq PrivateKey where
  eq = eqOrd

instance Ord PrivateKey where
  compare = compare `on` (unwrap >>> privateKey_asBytes)

instance EncodeAeson PrivateKey where
  encodeAeson = unwrap >>> privateKey_asBytes >>> encodeAeson

instance Show PrivateKey where
  show pk = "(PrivateKey " <> (privateKey_toBech32 <<< unwrap $ pk) <> ")"
