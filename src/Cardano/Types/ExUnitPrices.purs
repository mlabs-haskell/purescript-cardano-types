module Cardano.Types.ExUnitPrices
  ( ExUnitPrices(..)
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval as UnitInterval
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype ExUnitPrices = ExUnitPrices { memPrice :: UnitInterval, stepPrice :: UnitInterval }

derive instance Newtype ExUnitPrices _
derive instance Eq ExUnitPrices
derive instance Generic ExUnitPrices _

instance Show ExUnitPrices where
    show = genericShow

instance AsCbor ExUnitPrices where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.ExUnitPrices -> ExUnitPrices
fromCsl bw = let
  memPrice = UnitInterval.fromCsl (Csl.exUnitPrices_memPrice bw)
  stepPrice = UnitInterval.fromCsl (Csl.exUnitPrices_stepPrice bw)
  in
   (wrap { memPrice, stepPrice })

toCsl :: ExUnitPrices -> Csl.ExUnitPrices
toCsl (ExUnitPrices { memPrice, stepPrice }) =
  Csl.exUnitPrices_new (UnitInterval.toCsl memPrice) (UnitInterval.toCsl stepPrice)
