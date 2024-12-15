module Cardano.Types.PlutusScript where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson, decodeAeson, fromString)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, plutusScript_bytes, plutusScript_hash, plutusScript_new, toBytes)
import Cardano.Data.Lite as Csl
import Cardano.Types.Language (Language(PlutusV1, PlutusV2, PlutusV3))
import Cardano.Types.RawBytes (RawBytes)
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Array.NonEmpty as NEA
import Data.ByteArray (ByteArray)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript (ByteArray /\ Language)

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive instance Eq PlutusScript
derive instance Ord PlutusScript
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Arbitrary PlutusScript where
  arbitrary = oneOf $ NEA.cons'
    ( pure $ unsafePartial $ fromJust $ map plutusV1Script $ hush
        $ decodeAeson
        $ fromString "4d01000033222220051200120011"
    )
    [ pure $ unsafePartial $ fromJust $ map plutusV2Script $ hush
        $ decodeAeson
        $ fromString "4d010000deadbeef33222220051200120011"
    ]

instance Show PlutusScript where
  show = genericShow

plutusV1Script :: RawBytes -> PlutusScript
plutusV1Script ba = do
  PlutusScript $ unwrap ba /\ PlutusV1

plutusV2Script :: RawBytes -> PlutusScript
plutusV2Script ba =
  PlutusScript $ unwrap ba /\ PlutusV2

plutusV3Script :: RawBytes -> PlutusScript
plutusV3Script ba =
  PlutusScript $ unwrap ba /\ PlutusV3

instance AsCbor PlutusScript where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

languageToNumber :: Language -> Number
languageToNumber PlutusV1 = Int.toNumber 1
languageToNumber PlutusV2 = Int.toNumber 2
languageToNumber PlutusV3 = Int.toNumber 3

hash :: PlutusScript -> ScriptHash
hash script@(PlutusScript (_ /\ language)) = wrap $ plutusScript_hash (toCsl script)
  (languageToNumber language)

-- | Get raw Plutus script bytes
getBytes :: PlutusScript -> RawBytes
getBytes (PlutusScript (script /\ _)) = wrap script

toCsl :: PlutusScript -> Csl.PlutusScript
toCsl (PlutusScript (script /\ _)) =
  plutusScript_new script

-- TODO: extract language version
fromCsl :: Csl.PlutusScript -> PlutusScript
fromCsl ps =
  PlutusScript
    ( plutusScript_bytes ps
        /\ PlutusV1
    )
