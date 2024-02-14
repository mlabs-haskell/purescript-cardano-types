module Cardano.Types.ProposedProtocolParameterUpdates where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.Serialization.Lib (packMapContainer)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.GenesisHash (GenesisHash)
import Cardano.Types.ProtocolParamUpdate (ProtocolParamUpdate)
import Cardano.Types.ProtocolParamUpdate as ProtocolParamUpdate
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor.Strong ((***))
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

toCsl :: ProposedProtocolParameterUpdates -> Csl.ProposedProtocolParameterUpdates
toCsl = packMapContainer <<< map (unwrap *** ProtocolParamUpdate.toCsl) <<< Map.toUnfoldable <<< unwrap
