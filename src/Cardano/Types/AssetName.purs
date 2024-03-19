module Cardano.Types.AssetName where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(TypeMismatch)
  , caseAesonObject
  , encodeAeson
  , getField
  )
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.FromData (class FromData, fromData)
import Cardano.FromMetadata (class FromMetadata, fromMetadata)
import Cardano.Serialization.Lib (assetName_name, assetName_new, fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.ToData (class ToData, toData)
import Cardano.ToMetadata (class ToMetadata, toMetadata)
import Cardano.Types.Internal.Helpers (decodeUtf8)
import Data.ByteArray (ByteArray, byteArrayToHex, byteLength, hexToByteArray)
import Data.Either (Either(Left, Right), either, note)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String as String
import Data.TextEncoder (encodeUtf8)
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

-- | Corresponds to the Haskell instance at https://github.com/input-output-hk/plutus/blob/4fd86930f1dc628a816adf5f5d854b3fec578312/plutus-ledger-api/src/Plutus/V1/Ledger/Value.hs#L155:
instance DecodeAeson AssetName where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \aes -> do
      tkstr <- getField aes "unAssetName"
      case String.take 3 tkstr of
        "\x0000000x" -> do -- this is 3 characters '\NUL' '0' 'x'
          let stripped = String.drop 3 tkstr -- strip the \NUL followed by "0x"
          ba <-
            note
              (TypeMismatch $ "Expected base16 encoded string got " <> stripped)
              $ hexToByteArray stripped
          pure $ AssetName $ assetName_new ba
        "\x0\x0\x0" -> Right $ tkFromStr (String.drop 2 tkstr) -- if the original started with \NUL, we prepended 2 additional \NULs
        _ -> Right $ tkFromStr tkstr
    where
    tkFromStr :: String -> AssetName
    tkFromStr = AssetName <<< assetName_new <<< wrap <<< encodeUtf8

instance EncodeAeson AssetName where
  encodeAeson = encodeAeson <<< { "unAssetName": _ } <<< fromAssetName
    (\ba -> "\x0" <> "0x" <> byteArrayToHex ba)
    ( \s -> case String.take 1 s of
        "\x0" -> "\x0\x0" <> s
        _ -> s
    )

instance Show AssetName where
  show (AssetName tn) = "(mkAssetName " <> show (assetName_name tn) <> ")"

-- | Create a `AssetName` from a `ByteArray` since AssetName data constructor is
-- | not exported
mkAssetName :: ByteArray -> Maybe AssetName
mkAssetName byteArr
  | byteLength byteArr <= 32 = Just $ AssetName $ assetName_new byteArr
  | otherwise = Nothing
