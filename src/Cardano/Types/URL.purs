module Cardano.Types.URL where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.Serialization.Lib (url_new, url_url)
import Cardano.Serialization.Lib as Csl
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

newtype URL = URL String

derive instance Eq URL
derive instance Ord URL
derive instance Generic URL _
derive instance Newtype URL _
derive newtype instance EncodeAeson URL
derive newtype instance DecodeAeson URL

instance Show URL where
  show = genericShow

toCsl :: URL -> Csl.URL
toCsl (URL str) = url_new str

fromCsl :: Csl.URL -> URL
fromCsl = URL <<< url_url
