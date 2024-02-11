module Cardano.Types.ExUnits
  ( ExUnits(..)
  , fromCsl
  , toCsl
  ) where

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (BigNum)
import Control.Apply (map)
import Data.Eq (class Eq)
import Data.Function ((>>>))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Ord (class Ord)
import Data.Show (class Show)
import Data.Show.Generic (genericShow)

newtype ExUnits = ExUnits { mem :: BigNum, steps :: BigNum }

derive instance Newtype ExUnits _
derive instance Eq ExUnits
derive instance Ord ExUnits
derive instance Generic ExUnits _

instance Show ExUnits where
    show = genericShow

instance AsCbor ExUnits where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

fromCsl :: Csl.ExUnits -> ExUnits
fromCsl eu = let
  mem = wrap (Csl.exUnits_mem eu)
  steps = wrap (Csl.exUnits_steps eu)
  in
   (wrap { mem, steps })

toCsl :: ExUnits -> Csl.ExUnits
toCsl (ExUnits { mem, steps }) =
  Csl.exUnits_new (unwrap mem) (unwrap steps)
