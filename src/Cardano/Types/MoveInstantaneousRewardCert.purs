module Cardano.Types.MoveInstantaneousRewardsCert where

import Prelude

import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.MoveInstantaneousReward (MoveInstantaneousReward)
import Cardano.Types.MoveInstantaneousReward as MoveInstantaneousReward
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype MoveInstantaneousRewardsCert = MoveInstantaneousRewardsCert MoveInstantaneousReward

derive instance Generic MoveInstantaneousRewardsCert _
derive instance Newtype MoveInstantaneousRewardsCert _
derive instance Eq MoveInstantaneousRewardsCert
derive instance Ord MoveInstantaneousRewardsCert

instance Show MoveInstantaneousRewardsCert where
  show = genericShow

instance AsCbor MoveInstantaneousRewardsCert where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: MoveInstantaneousRewardsCert -> Csl.MoveInstantaneousRewardsCert
toCsl x = Csl.moveInstantaneousRewardsCert_new (MoveInstantaneousReward.toCsl (unwrap x))


fromCsl :: Csl.MoveInstantaneousRewardsCert -> MoveInstantaneousRewardsCert
fromCsl x = wrap (MoveInstantaneousReward.fromCsl (Csl.moveInstantaneousRewardsCert_moveInstantaneousReward x))
