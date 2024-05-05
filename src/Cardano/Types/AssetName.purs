module Cardano.Types.AssetName where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonString
  , encodeAeson
  )
import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.FromData (class FromData, fromData)
import Cardano.FromMetadata (class FromMetadata, fromMetadata)
import Cardano.Serialization.Lib (assetName_name, assetName_new, fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.Internal.Helpers (decodeUtf8)
import Data.ByteArray (ByteArray, byteLength, hexToByteArray)
import Data.Either (Either(Right, Left), either)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (resize)

newtype AssetName = AssetName Csl.AssetName

derive instance Generic AssetName _
derive instance Newtype AssetName _

instance Eq AssetName where
  eq = eq `on` encodeCbor

instance Ord AssetName where
  compare = compare `on` encodeCbor

instance AsCbor AssetName where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

instance ToData AssetName where
  toData = unAssetName >>> toData

instance FromData AssetName where
  fromData = mkAssetName <=< fromData

instance ToMetadata AssetName where
  toMetadata = toMetadata <<< unAssetName

instance FromMetadata AssetName where
  fromMetadata = mkAssetName <=< fromMetadata

instance Arbitrary AssetName where
  arbitrary = unsafePartial fromJust <<< mkAssetName <$> resize 32 arbitrary

unAssetName :: AssetName -> ByteArray
unAssetName = unwrap >>> assetName_name

fromAssetName
  :: forall (r :: Type). (ByteArray -> r) -> (String -> r) -> AssetName -> r
fromAssetName arrayHandler stringHandler (AssetName assetNameCsl) = either
  (const $ arrayHandler bs)
  stringHandler
  (decodeUtf8 bs)
  where
  bs = assetName_name assetNameCsl

instance DecodeAeson AssetName where
  decodeAeson = do
    maybe (Left $ TypeMismatch "Expected hex-encoded asset name") Right <<<
      caseAesonString Nothing
        (Just <=< decodeCbor <=< map wrap <<< hexToByteArray)

instance EncodeAeson AssetName where
  encodeAeson sh = encodeAeson $ encodeCbor sh

instance Show AssetName where
  show (AssetName tn) = "(mkAssetName " <> show (assetName_name tn) <> ")"

-- | Create a `AssetName` from a `ByteArray` since AssetName data constructor is
-- | not exported
mkAssetName :: ByteArray -> Maybe AssetName
mkAssetName byteArr
  | byteLength byteArr <= 32 = Just $ AssetName $ assetName_new byteArr
  | otherwise = Nothing
