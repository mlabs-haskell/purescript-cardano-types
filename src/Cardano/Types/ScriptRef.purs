module Cardano.Types.ScriptRef where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, JsonDecodeError(UnexpectedValue), decodeAeson, fromString, toStringifiedNumbersJson, (.:))
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib (fromBytes, scriptRef_nativeScript, scriptRef_newNativeScript, scriptRef_newPlutusScript, scriptRef_plutusScript, toBytes)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

data ScriptRef = NativeScriptRef NativeScript | PlutusScriptRef PlutusScript

derive instance Eq ScriptRef
derive instance Ord ScriptRef
derive instance Generic ScriptRef _

instance Arbitrary ScriptRef where
  arbitrary = genericArbitrary

instance Show ScriptRef where
  show = genericShow

instance AsCbor ScriptRef where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

instance EncodeAeson ScriptRef where
  encodeAeson = case _ of
    NativeScriptRef r -> encodeTagged' "NativeScriptRef" r
    PlutusScriptRef r -> encodeTagged' "PlutusScriptRef" r

instance DecodeAeson ScriptRef where
  decodeAeson = decodeAeson >=>
    \obj -> do
      tag <- obj .: "tag"
      case tag of
        "NativeScriptRef" -> do
          nativeScript <- obj .: "contents"
          pure $ NativeScriptRef nativeScript
        "PlutusScriptRef" -> do
          plutusScript <- obj .: "contents"
          pure $ PlutusScriptRef plutusScript
        tagValue -> do
          Left $ UnexpectedValue $ toStringifiedNumbersJson $ fromString
            tagValue

getNativeScript :: ScriptRef -> Maybe NativeScript
getNativeScript (NativeScriptRef nativeScript) = Just nativeScript
getNativeScript _ = Nothing

getPlutusScript :: ScriptRef -> Maybe PlutusScript
getPlutusScript (PlutusScriptRef plutusScript) = Just plutusScript
getPlutusScript _ = Nothing

toCsl :: ScriptRef -> Csl.ScriptRef
toCsl = case _ of
  NativeScriptRef ns -> scriptRef_newNativeScript $ NativeScript.toCsl ns
  PlutusScriptRef ps -> scriptRef_newPlutusScript $ PlutusScript.toCsl ps

fromCsl :: Csl.ScriptRef -> ScriptRef
fromCsl sr = unsafePartial $ fromJust $
  (NativeScriptRef <<< NativeScript.fromCsl <$> toMaybe (scriptRef_nativeScript sr)) <|>
    (PlutusScriptRef <<< PlutusScript.fromCsl <$> toMaybe (scriptRef_plutusScript sr))
