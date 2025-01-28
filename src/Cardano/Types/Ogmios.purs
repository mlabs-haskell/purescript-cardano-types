module Cardano.Types.Ogmios
  ( AdditionalUtxoSet(AdditionalUtxoSet)
  , OgmiosUtxoMap
  , OgmiosTxOutRef
  , OgmiosTxOut
  , OgmiosAddress
  ) where

import Prelude

import Aeson (class EncodeAeson, Aeson, encodeAeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.Types.AssetName (unAssetName)
import Cardano.Types.Bech32String (Bech32String)
import Cardano.Types.Internal.Helpers (encodeMap)
import Cardano.Types.Language (Language(..))
import Cardano.Types.NativeScript (NativeScript(..))
import Cardano.Types.PlutusScript (PlutusScript(..))
import Cardano.Types.ScriptRef (ScriptRef(..))
import Cardano.Types.Slot (Slot(..))
import Cardano.Types.Value (Value, getMultiAsset, valueToCoin)
import Data.ByteArray (byteArrayToHex)
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt as UInt
import JS.BigInt as BigInt

type OgmiosAddress = Bech32String

newtype AdditionalUtxoSet = AdditionalUtxoSet OgmiosUtxoMap

derive instance Newtype AdditionalUtxoSet _

derive newtype instance Show AdditionalUtxoSet

-- Ogmios tx input
type OgmiosTxOutRef =
  { txId :: String
  , index :: UInt.UInt
  }

type OgmiosTxOut =
  { address :: OgmiosAddress
  , value :: Value
  , datumHash :: Maybe String
  , datum :: Maybe String
  , script :: Maybe ScriptRef
  }

type OgmiosUtxoMap = Map OgmiosTxOutRef OgmiosTxOut

instance EncodeAeson AdditionalUtxoSet where
  encodeAeson (AdditionalUtxoSet m) =
    encodeAeson $ encode <$> utxos

    where
    utxos :: Array (OgmiosTxOutRef /\ OgmiosTxOut)
    utxos = Map.toUnfoldable m

    encode :: (OgmiosTxOutRef /\ OgmiosTxOut) -> Aeson
    encode (inp /\ out) = encodeAeson $
      { "transaction": { "id": inp.txId }
      , "index": inp.index
      , "address": out.address
      , "datumHash": out.datumHash
      , "datum": out.datum
      , "script": encodeScriptRef <$> out.script
      , "value": encodeValue out.value
      }

    encodeNativeScript :: NativeScript -> Aeson
    encodeNativeScript (ScriptPubkey s) =
      encodeAeson { "clause": "signature", "from": encodeAeson s }
    encodeNativeScript (ScriptAll ss) =
      encodeAeson { "clause": "all", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptAny ss) =
      encodeAeson { "clause": "any", "from": encodeNativeScript <$> ss }
    encodeNativeScript (ScriptNOfK n ss) =
      encodeAeson
        { "clause": "some"
        , "atLeast": BigInt.fromInt n
        , "from": encodeNativeScript <$> ss
        }
    encodeNativeScript (TimelockStart (Slot n)) =
      encodeAeson { "clause": "after", "slot": n }
    encodeNativeScript (TimelockExpiry (Slot n)) =
      encodeAeson { "clause": "before", "slot": n }

    encodeScriptRef :: ScriptRef -> Aeson
    encodeScriptRef (NativeScriptRef s) =
      encodeAeson
        { "language": "native"
        -- NOTE: We omit the cbor argument.
        , "json": (encodeNativeScript s)
        }
    encodeScriptRef (PlutusScriptRef (PlutusScript (script /\ lang))) =
      encodeAeson
        { "language":
            case lang of
              PlutusV1 -> "plutus:v1"
              PlutusV2 -> "plutus:v2"
              PlutusV3 -> "plutus:v3"
        , "cbor": byteArrayToHex script
        }

    encodeValue :: Value -> Aeson
    encodeValue value = encodeMap $ map encodeMap $ Map.union adaPart nonAdaPart
      where
      adaPart = Map.fromFoldable
        [ ( "ada" /\
              ( Map.fromFoldable
                  [ ("lovelace" /\ (value # valueToCoin # unwrap)) ]
              )
          )
        ]
      nonAdaPart = mapKeys (byteArrayToHex <<< unwrap <<< encodeCbor)
        $ map (mapKeys (byteArrayToHex <<< unAssetName))
        $ unwrap
        $ getMultiAsset value

      mapKeys :: forall k1 k2 a. Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
      mapKeys f = (Map.toUnfoldable :: Map k1 a -> Array (k1 /\ a)) >>> foldl
        (\m' (k /\ v) -> Map.insert (f k) v m')
        Map.empty
