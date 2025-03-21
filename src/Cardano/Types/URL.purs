module Cardano.Types.URL where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite (fromBytes, toBytes, url_new, url_url)
import Cardano.Data.Lite as Cdl
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype URL = URL String

derive instance Eq URL
derive instance Ord URL
derive instance Generic URL _
derive instance Newtype URL _
derive newtype instance EncodeAeson URL
derive newtype instance DecodeAeson URL

instance AsCbor URL where
  encodeCbor = toCdl >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map fromCdl

instance Show URL where
  show = genericShow

toCdl :: URL -> Cdl.URL
toCdl (URL str) = url_new str

fromCdl :: Cdl.URL -> URL
fromCdl = URL <<< url_url
