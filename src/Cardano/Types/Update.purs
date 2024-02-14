module Cardano.Types.Update where

import Prelude

import Cardano.Serialization.Lib (update_new)
import Cardano.Serialization.Lib as Csl
import Cardano.Type.Epoch (Epoch)
import Cardano.Types.ProposedProtocolParameterUpdates (ProposedProtocolParameterUpdates)
import Cardano.Types.ProposedProtocolParameterUpdates as ProposedProtocolParameterUpdates
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.UInt as UInt

newtype Update = Update
  { proposedProtocolParameterUpdates :: ProposedProtocolParameterUpdates
  , epoch :: Epoch
  }

derive newtype instance Eq Update
derive newtype instance Ord Update
derive instance Newtype Update _
derive instance Generic Update _

instance Show Update where
  show = genericShow

toCsl :: Update -> Csl.Update
toCsl (Update { proposedProtocolParameterUpdates, epoch }) =
  update_new
    (ProposedProtocolParameterUpdates.toCsl proposedProtocolParameterUpdates)
    (UInt.toNumber $ unwrap epoch)
