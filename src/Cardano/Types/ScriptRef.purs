module Cardano.Types.ScriptRef where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , JsonDecodeError(UnexpectedValue)
  , decodeAeson
  , fromString
  , toStringifiedNumbersJson
  , (.:)
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite
  ( fromBytes
  , scriptRef_nativeScript
  , scriptRef_newNativeScript
  , scriptRef_newPlutusScript_v1
  , scriptRef_newPlutusScript_v2
  , scriptRef_newPlutusScript_v3
  , scriptRef_plutusScript_v1
  , scriptRef_plutusScript_v2
  , scriptRef_plutusScript_v3
  , toBytes
  )
import Cardano.Data.Lite as Csl
import Cardano.Types.Internal.Helpers (encodeTagged')
import Cardano.Types.Language (Language(PlutusV1, PlutusV2, PlutusV3))
import Cardano.Types.Language as Language
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.PlutusScript (PlutusScript(PlutusScript))
import Cardano.Types.PlutusScript as PlutusScript
import Control.Alt ((<|>))
import Data.Either (Either(Left))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
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
  PlutusScriptRef ps@(PlutusScript (_ /\ lang)) -> case lang of
    Language.PlutusV1 -> scriptRef_newPlutusScript_v1 $ PlutusScript.toCsl ps
    Language.PlutusV2 -> scriptRef_newPlutusScript_v2 $ PlutusScript.toCsl ps
    Language.PlutusV3 -> scriptRef_newPlutusScript_v3 $ PlutusScript.toCsl ps

fromCsl :: Csl.ScriptRef -> ScriptRef
fromCsl sr = unsafePartial $ fromJust $
  (NativeScriptRef <<< NativeScript.fromCsl <$> toMaybe (scriptRef_nativeScript sr))
    <|> (PlutusScriptRef <<< setLanguage PlutusV1 <<< PlutusScript.fromCsl <$> toMaybe (scriptRef_plutusScript_v1 sr))
    <|> (PlutusScriptRef <<< setLanguage PlutusV2 <<< PlutusScript.fromCsl <$> toMaybe (scriptRef_plutusScript_v2 sr))
    <|> (PlutusScriptRef <<< setLanguage PlutusV3 <<< PlutusScript.fromCsl <$> toMaybe (scriptRef_plutusScript_v3 sr))
  where
  setLanguage :: Language -> PlutusScript -> PlutusScript
  setLanguage lang (PlutusScript (ba /\ _)) = PlutusScript (ba /\ lang)
