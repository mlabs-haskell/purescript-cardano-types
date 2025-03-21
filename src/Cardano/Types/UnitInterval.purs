module Cardano.Types.UnitInterval where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( unitInterval_denominator
  , unitInterval_new
  , unitInterval_numerator
  )
import Cardano.Data.Lite as Cdl
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
derive newtype instance EncodeAeson UnitInterval
derive newtype instance DecodeAeson UnitInterval

instance Show UnitInterval where
  show = genericShow

instance AsCbor UnitInterval where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

toCdl :: UnitInterval -> Cdl.UnitInterval
toCdl (UnitInterval { numerator, denominator }) = unitInterval_new (unwrap numerator) (unwrap denominator)

fromCdl :: Cdl.UnitInterval -> UnitInterval
fromCdl csl = UnitInterval
  { numerator: wrap $ unitInterval_numerator csl
  , denominator: wrap $ unitInterval_denominator csl
  }
