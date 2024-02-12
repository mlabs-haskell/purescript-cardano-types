module Cardano.Types.ScriptDataHash where

import Prelude

import Aeson (class EncodeAeson)
import Cardano.AsCbor (class AsCbor, encodeCbor)
import Cardano.Serialization.Lib (fromBytes, toBytes)
import Cardano.Serialization.Lib as Csl
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype ScriptDataHash = ScriptDataHash Csl.ScriptDataHash

derive instance Newtype ScriptDataHash _
derive instance Generic ScriptDataHash _

instance Eq ScriptDataHash where
  eq = eq `on` encodeCbor

instance Ord ScriptDataHash where
  compare = compare `on` encodeCbor

instance AsCbor ScriptDataHash where
  encodeCbor = unwrap >>> toBytes >>> wrap
  decodeCbor = unwrap >>> fromBytes >>> map wrap

derive newtype instance EncodeAeson ScriptDataHash

instance Show ScriptDataHash where
  show = genericShow
