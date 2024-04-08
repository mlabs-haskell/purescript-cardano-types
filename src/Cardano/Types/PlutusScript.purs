module Cardano.Types.PlutusScript where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( fromBytes
  , plutusScript_fromBytesWithVersion
  , plutusScript_hash
  , plutusScript_languageVersion
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Language (Language(PlutusV1, PlutusV2))
import Cardano.Types.Language as Language
import Cardano.Types.ScriptHash (ScriptHash)
import Data.ByteArray (ByteArray)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Arbitrary (genericArbitrary)

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript (ByteArray /\ Language)

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance Eq PlutusScript
derive newtype instance Ord PlutusScript
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Arbitrary PlutusScript where
  arbitrary = genericArbitrary

instance Show PlutusScript where
  show = genericShow

plutusV1Script :: ByteArray -> PlutusScript
plutusV1Script ba = PlutusScript (ba /\ PlutusV1)

plutusV2Script :: ByteArray -> PlutusScript
plutusV2Script ba = PlutusScript (ba /\ PlutusV2)

instance AsCbor PlutusScript where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

hash :: PlutusScript -> ScriptHash
hash = toCsl >>> plutusScript_hash >>> wrap

toCsl :: PlutusScript -> Csl.PlutusScript
toCsl (PlutusScript (bytes /\ lang)) =
  plutusScript_fromBytesWithVersion bytes $ Language.toCsl lang

fromCsl :: Csl.PlutusScript -> PlutusScript
fromCsl ps = PlutusScript (toBytes ps /\ Language.fromCsl (plutusScript_languageVersion ps))
