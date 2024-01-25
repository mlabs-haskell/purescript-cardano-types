module Cardano.AsCbor where

import Cardano.Types.CborBytes (CborBytes)
import Data.Maybe (Maybe)

-- | A typeclass for Cardano domain types that can encoded to and decoded from `CborBytes`
class AsCbor a where
  encodeCbor :: a -> CborBytes
  decodeCbor :: CborBytes -> Maybe a
