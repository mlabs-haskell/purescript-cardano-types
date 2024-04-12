module Cardano.Types.PlutusScript where

import Prelude

import Aeson
  ( class DecodeAeson
  , class EncodeAeson
  , decodeAeson
  , fromString
  )
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib
  ( fromBytes
  , plutusScript_bytes
  , plutusScript_hash
  , plutusScript_languageVersion
  , plutusScript_newWithVersion
  , toBytes
  )
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Internal.Helpers (eqOrd)
import Cardano.Types.Language (Language(PlutusV1, PlutusV2))
import Cardano.Types.Language as Language
import Cardano.Types.RawBytes (RawBytes)
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Array.NonEmpty as NEA
import Data.ByteArray (ByteArray)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
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
derive newtype instance DecodeAeson PlutusScript
derive newtype instance EncodeAeson PlutusScript

instance Eq PlutusScript where
  eq = eqOrd

instance Ord PlutusScript where
  compare (PlutusScript (s1 /\ l1)) (PlutusScript (s2 /\ l2)) =
    case compare l1 l2 of
      EQ -> compare s1 s2
      other -> other

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

instance AsCbor PlutusScript where
  encodeCbor = toCsl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCsl

hash :: PlutusScript -> ScriptHash
hash = toCsl >>> plutusScript_hash >>> wrap

-- | Get raw Plutus script bytes
getBytes :: PlutusScript -> RawBytes
getBytes (PlutusScript (script /\ _)) = wrap script

toCsl :: PlutusScript -> Csl.PlutusScript
toCsl (PlutusScript (script /\ lang)) =
  plutusScript_newWithVersion script (Language.toCsl lang)

fromCsl :: Csl.PlutusScript -> PlutusScript
fromCsl ps =
  PlutusScript
    ( plutusScript_bytes ps
        /\ Language.fromCsl (plutusScript_languageVersion ps)
    )
