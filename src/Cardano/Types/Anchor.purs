module Cardano.Types.Anchor
  ( Anchor(Anchor)
  , fromCsl
  , toCsl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Serialization.Lib as Csl
import Cardano.Types.AnchorDataHash (AnchorDataHash)
import Cardano.Types.URL (URL)
import Cardano.Types.URL (fromCsl, toCsl) as URL
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype Anchor = Anchor
  { url :: URL
  , dataHash :: AnchorDataHash
  }

derive instance Generic Anchor _
derive instance Newtype Anchor _
derive instance Eq Anchor
derive instance Ord Anchor
derive newtype instance EncodeAeson Anchor
derive newtype instance DecodeAeson Anchor

instance Show Anchor where
  show = genericShow

instance AsCbor Anchor where
  encodeCbor = wrap <<< Csl.toBytes <<< toCsl
  decodeCbor = map fromCsl <<< Csl.fromBytes <<< unwrap

toCsl :: Anchor -> Csl.Anchor
toCsl (Anchor { url, dataHash }) = Csl.anchor_new (URL.toCsl url) (unwrap dataHash)

fromCsl :: Csl.Anchor -> Anchor
fromCsl anchor =
  Anchor
    { url: URL.fromCsl $ Csl.anchor_url anchor
    , dataHash: wrap $ Csl.anchor_anchorDataHash anchor
    }
