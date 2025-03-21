module Cardano.Types.Language
  ( Language(PlutusV1, PlutusV2, PlutusV3)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , decodeAeson
  , encodeAeson
  , fromString
  , toStringifiedNumbersJson
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

data Language
  = PlutusV1
  | PlutusV2
  | PlutusV3

derive instance Generic Language _
derive instance Eq Language
derive instance Ord Language

instance Show Language where
  show = genericShow

instance EncodeAeson Language where
  encodeAeson = encodeAeson <<< case _ of
    PlutusV1 -> "PlutusV1"
    PlutusV2 -> "PlutusV2"
    PlutusV3 -> "PlutusV3"

instance DecodeAeson Language where
  decodeAeson = decodeAeson >=>
    case _ of
      "PlutusV1" -> pure PlutusV1
      "PlutusV2" -> pure PlutusV2
      "PlutusV3" -> pure PlutusV3
      other -> Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
        other

instance AsCbor Language where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

instance Arbitrary Language where
  arbitrary = genericArbitrary

fromCdl :: Cdl.Language -> Language
fromCdl lang =
  case Cdl.fromCslEnum (Cdl.language_kind lang) of
    Cdl.LanguageKind_PlutusV1 -> PlutusV1
    Cdl.LanguageKind_PlutusV2 -> PlutusV2
    Cdl.LanguageKind_PlutusV3 -> PlutusV3

toCdl :: Language -> Cdl.Language
toCdl = case _ of
  PlutusV1 -> Cdl.language_newPlutusV1
  PlutusV2 -> Cdl.language_newPlutusV2
  PlutusV3 -> Cdl.language_newPlutusV3
