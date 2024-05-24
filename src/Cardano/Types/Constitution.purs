module Cardano.Types.Constitution
  ( Constitution(Constitution)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.Anchor (Anchor)
import Cardano.Types.Anchor (fromCsl, toCsl) as Anchor
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

toCsl :: Constitution -> Csl.Constitution
toCsl (Constitution rec) =
  maybe (Csl.constitution_new anchor)
    (Csl.constitution_newWithScriptHash anchor <<< unwrap)
    rec.scriptHash
  where
  anchor = Anchor.toCsl rec.anchor

fromCsl :: Csl.Constitution -> Constitution
fromCsl constitution =
  Constitution
    { anchor: Anchor.fromCsl $ Csl.constitution_anchor constitution
    , scriptHash: wrap <$> toMaybe (Csl.constitution_scriptHash constitution)
    }
