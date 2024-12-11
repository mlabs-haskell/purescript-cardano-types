module Cardano.Types.Language
  ( Language(PlutusV1, PlutusV2, PlutusV3)
  , fromCsl
  , toCsl
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
import Cardano.Data.Lite as Csl
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
  encodeCbor = toCsl >>> Csl.toBytes >>> wrap
  decodeCbor = unwrap >>> Csl.fromBytes >>> map fromCsl

instance Arbitrary Language where
  arbitrary = genericArbitrary

fromCsl :: Csl.Language -> Language
fromCsl lang =
  case Csl.fromCslEnum (Csl.language_kind lang) of
    Csl.LanguageKind_PlutusV1 -> PlutusV1
    Csl.LanguageKind_PlutusV2 -> PlutusV2
    Csl.LanguageKind_PlutusV3 -> PlutusV3

toCsl :: Language -> Csl.Language
toCsl = case _ of
  PlutusV1 -> Csl.language_newPlutusV1
  PlutusV2 -> Csl.language_newPlutusV2
  PlutusV3 -> Csl.language_newPlutusV3
