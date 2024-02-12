module Cardano.Types.PoolRegistration where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.PoolParams (PoolParams(..))
import Cardano.Types.PoolParams as PoolParams
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PoolRegistration = PoolRegistration PoolParams

derive instance Generic PoolRegistration _
derive instance Newtype PoolRegistration _
derive instance Eq PoolRegistration
derive instance Ord PoolRegistration

instance Show PoolRegistration where
  show = genericShow

instance AsCbor PoolRegistration where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: PoolRegistration -> Csl.PoolRegistration
toCsl x = Csl.poolRegistration_new (PoolParams.toCsl (unwrap x))

fromCsl :: Csl.PoolRegistration -> PoolRegistration
fromCsl x = wrap (PoolParams.fromCsl (Csl.poolRegistration_poolParams x))
