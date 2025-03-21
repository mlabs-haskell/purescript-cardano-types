module Cardano.Types.PoolVotingThresholds
  ( PoolVotingThresholds(PoolVotingThresholds)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCdl, toCdl) as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
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

instance AsCbor PoolVotingThresholds where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: PoolVotingThresholds -> Cdl.PoolVotingThresholds
toCdl (PoolVotingThresholds rec) =
  Cdl.poolVotingThresholds_new
    (UnitInterval.toCdl rec.motionNoConfidence)
    (UnitInterval.toCdl rec.committeeNormal)
    (UnitInterval.toCdl rec.committeeNoConfidence)
    (UnitInterval.toCdl rec.hardForkInitiation)
    (UnitInterval.toCdl rec.securityRelevantThreshold)

fromCdl :: Cdl.PoolVotingThresholds -> PoolVotingThresholds
fromCdl pvt =
  PoolVotingThresholds
    { motionNoConfidence:
        UnitInterval.fromCdl $
          Cdl.poolVotingThresholds_motionNoConfidence pvt
    , committeeNormal:
        UnitInterval.fromCdl $
          Cdl.poolVotingThresholds_committeeNormal pvt
    , committeeNoConfidence:
        UnitInterval.fromCdl $
          Cdl.poolVotingThresholds_committeeNoConfidence pvt
    , hardForkInitiation:
        UnitInterval.fromCdl $
          Cdl.poolVotingThresholds_hardForkInitiation pvt
    , securityRelevantThreshold:
        UnitInterval.fromCdl $
          Cdl.poolVotingThresholds_securityRelevantThreshold pvt
    }
