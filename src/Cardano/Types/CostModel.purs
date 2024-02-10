module Cardano.Types.CostModel where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (compareViaCslBytes, eqOrd)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype CostModel = CostModel Csl.CostModel

derive instance Generic CostModel _
derive instance Newtype CostModel _

instance Eq CostModel where
  eq = eqOrd

instance Show CostModel where
  show = genericShow

instance Ord CostModel where
  compare = compareViaCslBytes `on` unwrap

instance AsCbor CostModel where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson CostModel
derive newtype instance DecodeAeson CostModel

