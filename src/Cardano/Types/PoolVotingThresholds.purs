module Cardano.Types.PoolVotingThresholds
  ( PoolVotingThresholds(PoolVotingThresholds)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.BigNum (one) as BigNum
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCsl, toCsl) as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)

newtype PoolVotingThresholds = PoolVotingThresholds
  { motionNoConfidence :: UnitInterval
  , committeeNormal :: UnitInterval
  , committeeNoConfidence :: UnitInterval
  , hardForkInitiation :: UnitInterval
  , securityRelevantThreshold :: UnitInterval
  -- ^ Some parameters are relevant to security properties of the
  -- system. Any proposal attempting to change such a parameter
  -- requires an additional vote of the SPOs, with the threshold
  -- Q5 `securityRelevantThreshold`
  }

derive instance Generic PoolVotingThresholds _
derive instance Newtype PoolVotingThresholds _
derive instance Eq PoolVotingThresholds
derive instance Ord PoolVotingThresholds
derive newtype instance EncodeAeson PoolVotingThresholds
derive newtype instance DecodeAeson PoolVotingThresholds

instance Show PoolVotingThresholds where
  show = genericShow

toCsl :: PoolVotingThresholds -> Csl.PoolVotingThresholds
toCsl (PoolVotingThresholds rec) =
  Csl.poolVotingThresholds_new
    (UnitInterval.toCsl rec.motionNoConfidence)
    (UnitInterval.toCsl rec.committeeNormal)
    (UnitInterval.toCsl rec.committeeNoConfidence)
    (UnitInterval.toCsl rec.hardForkInitiation)
    (UnitInterval.toCsl rec.securityRelevantThreshold)

fromCsl :: Csl.PoolVotingThresholds -> PoolVotingThresholds
fromCsl pvt =
  PoolVotingThresholds
    { motionNoConfidence:
        UnitInterval.fromCsl $
          Csl.poolVotingThresholds_motionNoConfidence pvt
    , committeeNormal:
        UnitInterval.fromCsl $
          Csl.poolVotingThresholds_committeeNormal pvt
    , committeeNoConfidence:
        UnitInterval.fromCsl $
          Csl.poolVotingThresholds_committeeNoConfidence pvt
    , hardForkInitiation:
        UnitInterval.fromCsl $
          Csl.poolVotingThresholds_hardForkInitiation pvt
    , securityRelevantThreshold:
        UnitInterval.fromCsl $
          Csl.poolVotingThresholds_securityRelevantThreshold pvt
    }
