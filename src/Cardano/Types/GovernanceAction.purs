module Cardano.Types.GovernanceAction where

import Prelude

import Cardano.Types.HardForkInitiationAction (HardForkInitiationAction)
import Cardano.Types.NewConstitutionAction (NewConstitutionAction)
import Cardano.Types.NoConfidenceAction (NoConfidenceAction)
import Cardano.Types.ParameterChangeAction (ParameterChangeAction)
import Cardano.Types.TreasuryWithdrawalsAction (TreasuryWithdrawalsAction)
import Cardano.Types.UpdateCommitteeAction (UpdateCommitteeAction)

data GovernanceAction
  = ChangePParams ParameterChangeAction
  | TriggerHF HardForkInitiationAction
  | TreasuryWdrl TreasuryWithdrawalsAction
  | NoConfidence NoConfidenceAction
  | NewCommittee UpdateCommitteeAction
  | NewConstitution NewConstitutionAction
  | Info
