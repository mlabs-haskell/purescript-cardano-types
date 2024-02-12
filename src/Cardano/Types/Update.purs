module Cardano.Types.Update where

import Cardano.Type.Epoch (Epoch)
import Cardano.Types.ProposedProtocolParameterUpdates
  ( ProposedProtocolParameterUpdates
  )

type Update =
  { proposedProtocolParameterUpdates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }
