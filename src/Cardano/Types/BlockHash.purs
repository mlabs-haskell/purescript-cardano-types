module Cardano.Types.BlockHash where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.AsCbor (class AsCbor)
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd, showFromCbor)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)

newtype BlockHash = BlockHash Csl.BlockHash

derive instance Generic BlockHash _
derive instance Newtype BlockHash _

instance Eq BlockHash where
  eq = eqOrd

instance AsCbor BlockHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson BlockHash
derive newtype instance DecodeAeson BlockHash

-- Custom Ord instance is provided to ensure lexicographical ordering
-- based on the hexadecimal string representation for consistency in ordering.
instance Ord BlockHash where
  compare = compareViaCslBytes `on` unwrap

instance Show BlockHash where
  show = unwrap >>> showFromCbor "BlockHash"
