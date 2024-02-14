module Cardano.Types.ExUnitPrices
  ( ExUnitPrices(ExUnitPrices)
  , toCsl
  , fromCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype ExUnitPrices = ExUnitPrices
  { memPrice :: UnitInterval
  , stepPrice :: UnitInterval
  }

derive instance Newtype ExUnitPrices _
derive instance Eq ExUnitPrices
derive instance Ord ExUnitPrices
derive instance Generic ExUnitPrices _
derive newtype instance EncodeAeson ExUnitPrices
derive newtype instance DecodeAeson ExUnitPrices

instance Show ExUnitPrices where
  show = genericShow

instance AsCbor ExUnitPrices where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.ExUnitPrices -> ExUnitPrices
fromCsl bw =
  let
    memPrice = UnitInterval.fromCsl (Csl.exUnitPrices_memPrice bw)
    stepPrice = UnitInterval.fromCsl (Csl.exUnitPrices_stepPrice bw)
  in
    (wrap { memPrice, stepPrice })

toCsl :: ExUnitPrices -> Csl.ExUnitPrices
toCsl (ExUnitPrices { memPrice, stepPrice }) =
  Csl.exUnitPrices_new (UnitInterval.toCsl memPrice) (UnitInterval.toCsl stepPrice)
