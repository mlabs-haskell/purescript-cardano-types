module Cardano.Types.ProposedProtocolParameterUpdates where

import Prelude

import Data.Map (Map)
import Aeson (class EncodeAeson)
import Cardano.Types.GenesisHash (GenesisHash)
import Cardano.Types.ProtocolParamUpdate (ProtocolParamUpdate)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype ProposedProtocolParameterUpdates =
  ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

derive instance Newtype ProposedProtocolParameterUpdates _

derive newtype instance Eq ProposedProtocolParameterUpdates
derive newtype instance Ord ProposedProtocolParameterUpdates

derive instance Generic ProposedProtocolParameterUpdates _

instance Show ProposedProtocolParameterUpdates where
  show = genericShow

derive newtype instance EncodeAeson ProposedProtocolParameterUpdates
