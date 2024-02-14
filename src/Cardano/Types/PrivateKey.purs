module Cardano.Types.PrivateKey where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.Serialization.Lib (privateKey_asBytes, privateKey_toBech32)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (eqOrd)
import Data.Generic.Rep (class Generic)
import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)

newtype PrivateKey = PrivateKey Csl.PrivateKey

derive instance Generic PrivateKey _
derive instance Newtype PrivateKey _

instance Eq PrivateKey where
  eq = eqOrd

instance Ord PrivateKey where
  compare = compare `on` (unwrap >>> privateKey_asBytes)

instance EncodeAeson PrivateKey where
  encodeAeson = unwrap >>> privateKey_asBytes >>> encodeAeson

instance Show PrivateKey where
  show pk = "(PrivateKey " <> (privateKey_toBech32 <<< unwrap $ pk) <> ")"
