module Cardano.Types.AnchorDataHash
  ( AnchorDataHash(AnchorDataHash)
  ) where

import Prelude

import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype AnchorDataHash = AnchorDataHash Csl.AnchorDataHash

derive instance Generic AnchorDataHash _
derive instance Newtype AnchorDataHash _

instance Eq AnchorDataHash where
  eq = eq `on` encodeCbor

instance Show AnchorDataHash where
  show = genericShow

instance AsCbor AnchorDataHash where
  encodeCbor = wrap <<< toBytes <<< unwrap
  decodeCbor = map wrap <<< fromBytes <<< unwrap
