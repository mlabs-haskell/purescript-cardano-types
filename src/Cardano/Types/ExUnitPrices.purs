module Cardano.Types.ExUnitPrices
  ( ExUnitPrices(ExUnitPrices)
  , toCdl
  , fromCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
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
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.ExUnitPrices -> ExUnitPrices
fromCdl bw =
  let
    memPrice = UnitInterval.fromCdl (Cdl.exUnitPrices_memPrice bw)
    stepPrice = UnitInterval.fromCdl (Cdl.exUnitPrices_stepPrice bw)
  in
    (wrap { memPrice, stepPrice })

toCdl :: ExUnitPrices -> Cdl.ExUnitPrices
toCdl (ExUnitPrices { memPrice, stepPrice }) =
  Cdl.exUnitPrices_new (UnitInterval.toCdl memPrice) (UnitInterval.toCdl stepPrice)
