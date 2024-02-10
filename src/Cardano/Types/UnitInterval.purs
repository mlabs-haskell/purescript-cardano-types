module Cardano.Types.UnitInterval where

import Prelude

import Cardano.Serialization.Lib
  ( unitInterval_denominator
  , unitInterval_new
  , unitInterval_numerator
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype UnitInterval = UnitInterval
  { numerator :: BigNum
  , denominator :: BigNum
  }

derive instance Newtype UnitInterval _
derive instance Eq UnitInterval
derive instance Ord UnitInterval
derive instance Generic UnitInterval _

instance Show UnitInterval where
  show = genericShow

instance AsCbor UnitInterval where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: UnitInterval -> Csl.UnitInterval
toCsl (UnitInterval { numerator, denominator }) = unitInterval_new (unwrap numerator) (unwrap denominator)

fromCsl :: Csl.UnitInterval -> UnitInterval
fromCsl csl = UnitInterval
  { numerator: wrap $ unitInterval_numerator csl
  , denominator: wrap $ unitInterval_denominator csl
  }

