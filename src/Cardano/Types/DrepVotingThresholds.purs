module Cardano.Types.DrepVotingThresholds
  ( DrepVotingThresholds(DrepVotingThresholds)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCsl, toCsl) as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype DrepVotingThresholds = DrepVotingThresholds
  { motionNoConfidence :: UnitInterval
  , committeeNormal :: UnitInterval
  , committeeNoConfidence :: UnitInterval
  , updateConstitution :: UnitInterval
  , hardForkInitiation :: UnitInterval
  , ppNetworkGroup :: UnitInterval
  , ppEconomicGroup :: UnitInterval
  , ppTechnicalGroup :: UnitInterval
  , ppGovernanceGroup :: UnitInterval
  , treasuryWithdrawal :: UnitInterval
  }

derive instance Generic DrepVotingThresholds _
derive instance Newtype DrepVotingThresholds _
derive instance Eq DrepVotingThresholds
derive instance Ord DrepVotingThresholds
derive newtype instance EncodeAeson DrepVotingThresholds
derive newtype instance DecodeAeson DrepVotingThresholds

instance Show DrepVotingThresholds where
  show = genericShow

toCsl :: DrepVotingThresholds -> Csl.DrepVotingThresholds
toCsl (DrepVotingThresholds rec) =
  Csl.drepVotingThresholds_new
    (UnitInterval.toCsl rec.motionNoConfidence)
    (UnitInterval.toCsl rec.committeeNormal)
    (UnitInterval.toCsl rec.committeeNoConfidence)
    (UnitInterval.toCsl rec.updateConstitution)
    (UnitInterval.toCsl rec.hardForkInitiation)
    (UnitInterval.toCsl rec.ppNetworkGroup)
    (UnitInterval.toCsl rec.ppEconomicGroup)
    (UnitInterval.toCsl rec.ppTechnicalGroup)
    (UnitInterval.toCsl rec.ppGovernanceGroup)
    (UnitInterval.toCsl rec.treasuryWithdrawal)

fromCsl :: Csl.DrepVotingThresholds -> DrepVotingThresholds
fromCsl dvt =
  DrepVotingThresholds
    { motionNoConfidence:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_motionNoConfidence dvt
    , committeeNormal:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_committeeNormal dvt
    , committeeNoConfidence:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_committeeNoConfidence dvt
    , updateConstitution:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_updateConstitution dvt
    , hardForkInitiation:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_hardForkInitiation dvt
    , ppNetworkGroup:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_ppNetworkGroup dvt
    , ppEconomicGroup:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_ppEconomicGroup dvt
    , ppTechnicalGroup:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_ppTechnicalGroup dvt
    , ppGovernanceGroup:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_ppGovernanceGroup dvt
    , treasuryWithdrawal:
        UnitInterval.fromCsl $
          Csl.drepVotingThresholds_treasuryWithdrawal dvt
    }
