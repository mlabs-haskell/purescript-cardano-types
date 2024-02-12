module Cardano.Types.PoolRetirement where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Ed25519KeyHash (Ed25519KeyHash(..))
import Cardano.Types.PoolParams (PoolParams(..))
import Cardano.Types.PoolParams as PoolParams
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype PoolRetirement = PoolRetirement { poolKeyhash :: Ed25519KeyHash, epoch :: Number }

derive instance Generic PoolRetirement _
derive instance Newtype PoolRetirement _
derive instance Eq PoolRetirement
derive instance Ord PoolRetirement

instance Show PoolRetirement where
  show = genericShow

instance AsCbor PoolRetirement where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: PoolRetirement -> Csl.PoolRetirement
toCsl (PoolRetirement {poolKeyhash, epoch}) = Csl.poolRetirement_new (unwrap poolKeyhash) epoch

fromCsl :: Csl.PoolRetirement -> PoolRetirement
fromCsl x =
  let
    poolKeyhash = wrap (Csl.poolRetirement_poolKeyhash x)
    epoch = Csl.poolRetirement_epoch x
  in PoolRetirement {poolKeyhash, epoch}
