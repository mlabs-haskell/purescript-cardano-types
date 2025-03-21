module Cardano.Types.DRepVotingThresholds
  ( DRepVotingThresholds(DRepVotingThresholds)
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
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: DRepVotingThresholds -> Cdl.DRepVotingThresholds
toCdl (DRepVotingThresholds rec) =
  Cdl.dRepVotingThresholds_new
    (UnitInterval.toCdl rec.motionNoConfidence)
    (UnitInterval.toCdl rec.committeeNormal)
    (UnitInterval.toCdl rec.committeeNoConfidence)
    (UnitInterval.toCdl rec.updateConstitution)
    (UnitInterval.toCdl rec.hardForkInitiation)
    (UnitInterval.toCdl rec.ppNetworkGroup)
    (UnitInterval.toCdl rec.ppEconomicGroup)
    (UnitInterval.toCdl rec.ppTechnicalGroup)
    (UnitInterval.toCdl rec.ppGovernanceGroup)
    (UnitInterval.toCdl rec.treasuryWithdrawal)

fromCdl :: Cdl.DRepVotingThresholds -> DRepVotingThresholds
fromCdl dvt =
  DRepVotingThresholds
    { motionNoConfidence:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_motionNoConfidence dvt
    , committeeNormal:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_committeeNormal dvt
    , committeeNoConfidence:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_committeeNoConfidence dvt
    , updateConstitution:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_updateConstitution dvt
    , hardForkInitiation:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_hardForkInitiation dvt
    , ppNetworkGroup:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_ppNetworkGroup dvt
    , ppEconomicGroup:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_ppEconomicGroup dvt
    , ppTechnicalGroup:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_ppTechnicalGroup dvt
    , ppGovernanceGroup:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_ppGovernanceGroup dvt
    , treasuryWithdrawal:
        UnitInterval.fromCdl $
          Cdl.dRepVotingThresholds_treasuryWithdrawal dvt
    }
