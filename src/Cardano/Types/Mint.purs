module Cardano.Types.Mint where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, encodeAeson)
import Cardano.Serialization.Lib (packMapContainer, unpackMapContainerToMapWith)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))

newtype Mint = Mint (Map ScriptHash (Map AssetName Int.Int))

derive instance Generic Mint _
derive newtype instance Eq Mint
derive instance Newtype Mint _
-- no Ord instance to prevent confusion

instance Show Mint where
  show = genericShow

instance EncodeAeson Mint where
  encodeAeson = toCsl >>> encodeAeson

instance DecodeAeson Mint where
  decodeAeson = map fromCsl <<< decodeAeson

toCsl :: Mint -> Csl.Mint
toCsl (Mint mp) = packMapContainer $ Map.toUnfoldable mp <#> \(scriptHash /\ mintAssets) ->
  unwrap scriptHash /\
    packMapContainer do
      Map.toUnfoldable mintAssets <#> \(assetName /\ quantity) -> do
        unwrap assetName /\ unwrap quantity

fromCsl :: Csl.Mint -> Mint
fromCsl = wrap <<< unpackMapContainerToMapWith wrap
  (unpackMapContainerToMapWith wrap wrap)
