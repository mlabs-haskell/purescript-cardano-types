module Cardano.Types.Anchor
  ( Anchor(Anchor)
  , fromCdl
  , toCdl
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.AnchorDataHash (AnchorDataHash)
import Cardano.Types.URL (URL)
import Cardano.Types.URL (fromCdl, toCdl) as URL
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
  encodeCbor = wrap <<< Cdl.toBytes <<< toCdl
  decodeCbor = map fromCdl <<< Cdl.fromBytes <<< unwrap

toCdl :: Anchor -> Cdl.Anchor
toCdl (Anchor { url, dataHash }) = Cdl.anchor_new (URL.toCdl url) (unwrap dataHash)

fromCdl :: Cdl.Anchor -> Anchor
fromCdl anchor =
  Anchor
    { url: URL.fromCdl $ Cdl.anchor_url anchor
    , dataHash: wrap $ Cdl.anchor_anchorDataHash anchor
    }
