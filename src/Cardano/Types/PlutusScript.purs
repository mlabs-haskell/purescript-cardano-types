module Cardano.Types.PlutusScript where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , fromString
  )
import Cardano.AsCbor (class AsCbor, encodeCbor)
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
import Data.Array.NonEmpty as NEA
import Data.ByteArray (ByteArray)
import Data.Either (hush)
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary)
import Test.QuickCheck.Gen (oneOf)

-- | Corresponds to "Script" in Plutus
newtype PlutusScript = PlutusScript (Csl.PlutusScript /\ Language)

derive instance Generic PlutusScript _
derive instance Newtype PlutusScript _
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Eq PlutusScript where
  eq = eq `on` encodeCbor

instance Ord PlutusScript where
  compare = compare `on` encodeCbor

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

plutusV1Script :: ByteArray -> PlutusScript
plutusV1Script ba = PlutusScript (plutusScript_fromBytesWithVersion ba (Language.toCsl PlutusV1) /\ PlutusV1)

plutusV2Script :: ByteArray -> PlutusScript
plutusV2Script ba = PlutusScript (plutusScript_fromBytesWithVersion ba (Language.toCsl PlutusV2) /\ PlutusV2)

instance AsCbor PlutusScript where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

hash :: PlutusScript -> ScriptHash
hash = toCsl >>> plutusScript_hash >>> wrap

toCsl :: PlutusScript -> Csl.PlutusScript
toCsl (PlutusScript (script /\ _lang)) =
  script

fromCsl :: Csl.PlutusScript -> PlutusScript
fromCsl ps = PlutusScript (ps /\ Language.fromCsl (plutusScript_languageVersion ps))
