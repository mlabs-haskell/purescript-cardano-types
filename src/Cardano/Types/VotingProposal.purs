module Cardano.Types.VotingProposal
  ( VotingProposal(VotingProposal)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCdl, toCdl) as Anchor
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.GovernanceAction (GovernanceAction)
import Cardano.Types.GovernanceAction (fromCdl, toCdl) as GovernanceAction
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress (fromCdl, toCdl) as RewardAddress
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype VotingProposal = VotingProposal
  { govAction :: GovernanceAction
  , anchor :: Anchor
  -- ^ further information about the proposal
  , deposit :: BigNum
  -- ^ for a proposal to be valid, the deposit must be set to the
  -- current value of `govActionDeposit`
  , returnAddr :: RewardAddress
  -- ^ deposit will be returned to this address
  }

derive instance Generic VotingProposal _
derive instance Newtype VotingProposal _
derive instance Eq VotingProposal
derive instance Ord VotingProposal
derive newtype instance EncodeAeson VotingProposal
derive newtype instance DecodeAeson VotingProposal

instance Show VotingProposal where
  show = genericShow

instance AsCbor VotingProposal where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: VotingProposal -> Cdl.VotingProposal
toCdl (VotingProposal rec) =
  Cdl.votingProposal_new (GovernanceAction.toCdl rec.govAction) (Anchor.toCdl rec.anchor)
    (RewardAddress.toCdl rec.returnAddr)
    (unwrap rec.deposit)

fromCdl :: Cdl.VotingProposal -> VotingProposal
fromCdl proposal =
  VotingProposal
    { govAction:
        GovernanceAction.fromCdl $
          Cdl.votingProposal_governanceAction proposal
    , anchor:
        Anchor.fromCdl $
          Cdl.votingProposal_anchor proposal
    , deposit:
        wrap $
          Cdl.votingProposal_deposit proposal
    , returnAddr:
        RewardAddress.fromCdl $
          Cdl.votingProposal_rewardAccount proposal
    }
