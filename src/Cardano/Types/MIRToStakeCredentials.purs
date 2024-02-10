module Cardano.Types.MIRToStakeCredentials where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainerToMapWith)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Credential as Credential
import Cardano.Types.Int as Int
import Cardano.Types.Internal.Helpers (encodeMap)
import Cardano.Types.StakeCredential (StakeCredential)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor.Strong ((***))
import Data.Show.Generic (genericShow)

newtype MIRToStakeCredentials = MIRToStakeCredentials
  (Map StakeCredential Int.Int)

derive instance Eq MIRToStakeCredentials
derive instance Ord MIRToStakeCredentials
derive instance Newtype MIRToStakeCredentials _
derive instance Generic MIRToStakeCredentials _

instance Show MIRToStakeCredentials where
  show = genericShow

instance EncodeAeson MIRToStakeCredentials where
  encodeAeson (MIRToStakeCredentials r) = encodeMap r

instance AsCbor MIRToStakeCredentials where
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

toCsl :: MIRToStakeCredentials -> Csl.MIRToStakeCredentials
toCsl =
  packMapContainer
    <<< map (Credential.toCsl <<< unwrap *** unwrap)
    <<< Map.toUnfoldable
    <<<
      unwrap

fromCsl :: Csl.MIRToStakeCredentials -> MIRToStakeCredentials
fromCsl = unpackMapContainerToMapWith (Credential.fromCsl >>> wrap) wrap >>> wrap
