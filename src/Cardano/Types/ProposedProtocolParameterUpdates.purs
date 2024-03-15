module Cardano.Types.ProposedProtocolParameterUpdates where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainerToMapWith)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.GenesisHash (GenesisHash)
import Cardano.Types.ProtocolParamUpdate (ProtocolParamUpdate)
import Cardano.Types.ProtocolParamUpdate as ProtocolParamUpdate
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)

newtype ProposedProtocolParameterUpdates =
  ProposedProtocolParameterUpdates (Map GenesisHash ProtocolParamUpdate)

derive instance Newtype ProposedProtocolParameterUpdates _

derive newtype instance Eq ProposedProtocolParameterUpdates
derive newtype instance Ord ProposedProtocolParameterUpdates
derive newtype instance EncodeAeson ProposedProtocolParameterUpdates
derive newtype instance DecodeAeson ProposedProtocolParameterUpdates

instance AsCbor ProposedProtocolParameterUpdates where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

derive instance Generic ProposedProtocolParameterUpdates _

instance Show ProposedProtocolParameterUpdates where
  show = genericShow

toCsl :: ProposedProtocolParameterUpdates -> Csl.ProposedProtocolParameterUpdates
toCsl = packMapContainer <<< map (unwrap *** ProtocolParamUpdate.toCsl) <<< Map.toUnfoldable <<< unwrap

fromCsl :: Csl.ProposedProtocolParameterUpdates -> ProposedProtocolParameterUpdates
fromCsl = wrap <<< unpackMapContainerToMapWith wrap ProtocolParamUpdate.fromCsl
