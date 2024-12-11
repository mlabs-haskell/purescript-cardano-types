module Cardano.Types.DRepVotingThresholds
  ( DRepVotingThresholds(DRepVotingThresholds)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.UnitInterval (UnitInterval)
import Cardano.Types.UnitInterval (fromCsl, toCsl) as UnitInterval
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype DRepVotingThresholds = DRepVotingThresholds
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

derive instance Generic DRepVotingThresholds _
derive instance Newtype DRepVotingThresholds _
derive instance Eq DRepVotingThresholds
derive instance Ord DRepVotingThresholds
derive newtype instance EncodeAeson DRepVotingThresholds
derive newtype instance DecodeAeson DRepVotingThresholds

instance Show DRepVotingThresholds where
  show = genericShow

instance AsCbor DRepVotingThresholds where
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: DRepVotingThresholds -> Csl.DRepVotingThresholds
toCsl (DRepVotingThresholds rec) =
  Csl.dRepVotingThresholds_new
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

fromCsl :: Csl.DRepVotingThresholds -> DRepVotingThresholds
fromCsl dvt =
  DRepVotingThresholds
    { motionNoConfidence:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_motionNoConfidence dvt
    , committeeNormal:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_committeeNormal dvt
    , committeeNoConfidence:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_committeeNoConfidence dvt
    , updateConstitution:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_updateConstitution dvt
    , hardForkInitiation:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_hardForkInitiation dvt
    , ppNetworkGroup:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_ppNetworkGroup dvt
    , ppEconomicGroup:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_ppEconomicGroup dvt
    , ppTechnicalGroup:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_ppTechnicalGroup dvt
    , ppGovernanceGroup:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_ppGovernanceGroup dvt
    , treasuryWithdrawal:
        UnitInterval.fromCsl $
          Csl.dRepVotingThresholds_treasuryWithdrawal dvt
    }
