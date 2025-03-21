module Cardano.Types.ExUnits
  ( ExUnits(ExUnits)
  , fromCdl
  , toCdl
  , empty
  ) where

import Prelude

import Aeson (class DecodeAeson, class EncodeAeson)
import Cardano.AsCbor (class AsCbor)
import Cardano.Data.Lite as Cdl
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)

newtype ExUnits = ExUnits { mem :: BigNum, steps :: BigNum }

derive instance Newtype ExUnits _
derive instance Eq ExUnits
derive instance Ord ExUnits
derive instance Generic ExUnits _
derive newtype instance EncodeAeson ExUnits
derive newtype instance DecodeAeson ExUnits

instance Partial => Semigroup ExUnits where
  append (ExUnits { mem: mem1, steps: steps1 }) (ExUnits { mem: mem2, steps: steps2 }) =
    unsafePerformEffect $ maybe (throw "ExUnits.append: numeric overflow") pure do
      mem <- BigNum.add mem1 mem2
      steps <- BigNum.add steps1 steps2
      pure $ ExUnits { mem, steps }

instance Partial => Monoid ExUnits where
  mempty = empty

empty :: ExUnits
empty = ExUnits { mem: BigNum.zero, steps: BigNum.zero }

instance Show ExUnits where
  show = genericShow

instance AsCbor ExUnits where
  encodeCbor = toCdl >>> Cdl.toBytes >>> wrap
  decodeCbor = unwrap >>> Cdl.fromBytes >>> map fromCdl

fromCdl :: Cdl.ExUnits -> ExUnits
fromCdl eu =
  let
    mem = wrap (Cdl.exUnits_mem eu)
    steps = wrap (Cdl.exUnits_steps eu)
  in
    (wrap { mem, steps })

toCdl :: ExUnits -> Cdl.ExUnits
toCdl (ExUnits { mem, steps }) =
  Cdl.exUnits_new (unwrap mem) (unwrap steps)
