module Cardano.Types.OutputDatum
  ( OutputDatum(OutputDatumHash, OutputDatum)
  , outputDatumDataHash
  , outputDatumDatum
  , pprintOutputDatum
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(AtKey, Named, TypeMismatch, UnexpectedValue)
  , caseAesonObject
  , fromString
  , toStringifiedNumbersJson
  , (.:)
  )
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Data.Lite as Cdl
import Cardano.FromData (class FromData, genericFromData)
import Cardano.Plutus.DataSchema (class HasPlutusSchema, type (:+), type (:=), type (@@), PNil, S, Z)
import Cardano.ToData (class ToData, genericToData)
import Cardano.Types.DataHash (DataHash)
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.PlutusData (PlutusData, pprintPlutusData)
import Cardano.Types.PlutusData as PlutusData
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Log.Tag (TagSet, tag, tagSetTag)
import Data.Log.Tag as TagSet
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

data OutputDatum = OutputDatumHash DataHash | OutputDatum PlutusData

derive instance Generic OutputDatum _
derive instance Eq OutputDatum
derive instance Ord OutputDatum

instance Show OutputDatum where
  show = genericShow

instance
  HasPlutusSchema OutputDatum
    ( "OutputDatumHash"
        := PNil
        @@ Z
        :+ "OutputDatum"
        := PNil
        @@ (S Z)
        :+ PNil
    )

instance ToData OutputDatum where
  toData = genericToData

instance FromData OutputDatum where
  fromData = genericFromData

instance EncodeAeson OutputDatum where
  encodeAeson = case _ of
    OutputDatumHash r -> encodeTagged' "OutputDatumHash" r
    OutputDatum r -> encodeTagged' "OutputDatum" r

instance DecodeAeson OutputDatum where
  decodeAeson = caseAesonObject (Left $ TypeMismatch "Expected object") $
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "OutputDatumHash" -> do
          dataHash <- obj .: "contents"
          pure $ OutputDatumHash dataHash
        "OutputDatum" -> do
          datum <- obj .: "contents"
          pure $ OutputDatum datum
        tagValue -> do
          Left $ Named "OutputDatum"
            $ AtKey "tag"
            $ UnexpectedValue
            $ toStringifiedNumbersJson
            $ fromString tagValue

instance AsCbor OutputDatum where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

instance Arbitrary OutputDatum where
  arbitrary = genericArbitrary

fromCdl :: Cdl.OutputDatum -> OutputDatum
fromCdl cslOd = case toMaybe (Cdl.outputDatum_dataHash cslOd) of
  Just hash -> OutputDatumHash (wrap hash)
  Nothing -> case toMaybe (Cdl.outputDatum_data cslOd) of
    Just dat -> OutputDatum (PlutusData.fromCdl dat)
    Nothing -> unsafePerformEffect $ throw "Cardano.Types.OutputDatum.fromCdl: unknown kind"

toCdl :: OutputDatum -> Cdl.OutputDatum
toCdl = case _ of
  OutputDatumHash hash -> Cdl.outputDatum_newDataHash (unwrap hash)
  OutputDatum datum -> Cdl.outputDatum_newData (PlutusData.toCdl datum)

pprintOutputDatum :: OutputDatum -> TagSet
pprintOutputDatum = TagSet.fromArray <<< case _ of
  OutputDatumHash hash ->
    [ "datumHash" `tag` byteArrayToHex (unwrap $ encodeCbor hash) ]
  OutputDatum d ->
    [ "datum" `tagSetTag` pprintPlutusData d ]

outputDatumDataHash :: OutputDatum -> Maybe DataHash
outputDatumDataHash (OutputDatumHash hash) = Just hash
outputDatumDataHash _ = Nothing

outputDatumDatum :: OutputDatum -> Maybe PlutusData
outputDatumDatum (OutputDatum datum) = Just datum
outputDatumDatum _ = Nothing
