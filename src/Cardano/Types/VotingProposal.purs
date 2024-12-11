module Cardano.Types.VotingProposal
  ( VotingProposal(VotingProposal)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Csl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCsl, toCsl) as Anchor
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.GovernanceAction (GovernanceAction)
import Cardano.Types.GovernanceAction (fromCsl, toCsl) as GovernanceAction
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.RewardAddress (fromCsl, toCsl) as RewardAddress
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
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: VotingProposal -> Csl.VotingProposal
toCsl (VotingProposal rec) =
  Csl.votingProposal_new (GovernanceAction.toCsl rec.govAction) (Anchor.toCsl rec.anchor)
    (RewardAddress.toCsl rec.returnAddr)
    (unwrap rec.deposit)

fromCsl :: Csl.VotingProposal -> VotingProposal
fromCsl proposal =
  VotingProposal
    { govAction:
        GovernanceAction.fromCsl $
          Csl.votingProposal_governanceAction proposal
    , anchor:
        Anchor.fromCsl $
          Csl.votingProposal_anchor proposal
    , deposit:
        wrap $
          Csl.votingProposal_deposit proposal
    , returnAddr:
        RewardAddress.fromCsl $
          Csl.votingProposal_rewardAccount proposal
    }
