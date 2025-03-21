module Cardano.Types.Constitution
  ( Constitution(Constitution)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCdl, toCdl) as Anchor
import Cardano.Types.ScriptHash (ScriptHash)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)
import Data.Show.Generic (genericShow)

newtype Constitution = Constitution
  { anchor :: Anchor
  , scriptHash :: Maybe ScriptHash
  }

derive instance Generic Constitution _
derive instance Newtype Constitution _
derive instance Eq Constitution
derive instance Ord Constitution
derive newtype instance EncodeAeson Constitution
derive newtype instance DecodeAeson Constitution

instance Show Constitution where
  show = genericShow

instance AsCbor Constitution where
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: Constitution -> Cdl.Constitution
toCdl (Constitution rec) =
  maybe (Cdl.constitution_new anchor)
    (Cdl.constitution_newWithScriptHash anchor <<< unwrap)
    rec.scriptHash
  where
  anchor = Anchor.toCdl rec.anchor

fromCdl :: Cdl.Constitution -> Constitution
fromCdl constitution =
  Constitution
    { anchor: Anchor.fromCdl $ Cdl.constitution_anchor constitution
    , scriptHash: wrap <$> toMaybe (Cdl.constitution_scriptHash constitution)
    }
